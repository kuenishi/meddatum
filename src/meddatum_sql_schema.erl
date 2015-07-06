%%% @doc
%%%  meddatum json-schema for presto-riak
%%% @end
-module(meddatum_sql_schema).

-include("meddatum.hrl").

-export([create/1, check/0, setup/0]).

%% meta structures are here
-define(PRESTO_SCHEMA_BUCKET, <<"__presto_schema">>).
%% all tables' name should be here
-define(PRESTO_SCHEMA_KEY, <<"__schema">>).

static_tabledef() ->
    SSMIXTable0 = {[
                    {<<"name">>, ?SSMIX_PATIENTS_BUCKET},
                    {<<"columns">>, [atom_to_binary({Col})||Col<-hl7:columns()]}
                   ] ++ hl7:subtables() },
    jsone:encode(SSMIXTable0, [native_utf8]).

normal_tabledef() ->
    SSMIXTable1 = {[
                    {<<"name">>, ?SSMIX_BUCKET},
                    {<<"columns">>, [atom_to_binary({Col})||Col<-hl7:columns()]}
                   ] ++ hl7:subtables() },
    jsone:encode(SSMIXTable1, [native_utf8]).

recept_tabledef() ->
    ReceptTable = {[
                    {<<"name">>, ?RECEPT_BUCKET},
                    {<<"columns">>, [atom_to_binary({C})||C<-rezept:columns()]}
                   ] ++ rezept:subtables()},
    jsone:encode(ReceptTable, [native_utf8]).

create(Type) ->
    ok = io:setopts([{encoding,utf8}]),
    Text= case Type of
              static -> static_tabledef();
              ssmix ->  normal_tabledef();
              recept -> recept_tabledef()
          end,
    io:format("~ts~n", [Text]).

check() ->
    {ok, #context{logger=Logger,
                  riakc=C} = Context} = meddatum_console:setup(),
    try
        %% check presto-riak enabled or not
        {ok, Tables} = get_tables(C, Logger, ?BUCKET_TYPE),

        %% table definition is in {<<"md">>, <<"__presto_schema">>}, <<"tablename">>
        io:format("Checking schema for static ssmix records..."),
        BucketName0 = ?SSMIX_PATIENTS_BUCKET,
        io:format("~p, ", [lists:member(BucketName0, Tables)]),
        Tabledef0 = static_tabledef(),
        Tabledef1 = get_tabledef(C, BucketName0),
        io:format("~p~n", [Tabledef0 =:= Tabledef1]),

        io:format("Checking schema for normal ssmix records..."),
        BucketName1 = ?SSMIX_BUCKET,
        io:format("~p, ", [lists:member(BucketName1, Tables)]),
        Tabledef00 = normal_tabledef(),
        Tabledef01 = get_tabledef(C, BucketName1),
        io:format("~p~n", [Tabledef00 =:= Tabledef01]),

        io:format("Checking schema for rezept..."),
        BucketName2 = ?RECEPT_BUCKET,
        io:format("~p, ", [lists:member(BucketName2, Tables)]),
        Tabledef10 = recept_tabledef(),
        Tabledef11 = get_tabledef(C, BucketName2),
        io:format("~p~n", [Tabledef10 =:= Tabledef11]),
        ok

    after
            meddatum_console:teardown(Context)
    end.

setup() ->
    {ok, #context{logger=Logger,
                  riakc=C} = Context} = meddatum_console:setup(),
    try
        %% check presto-riak enabled or not
        case get_tables(C, Logger, ?BUCKET_TYPE) of
            {ok, Tables} ->

                io:format("Updating schema for static ssmix records ..."),
                BucketName0 = ?SSMIX_PATIENTS_BUCKET,
                case lists:member(BucketName0, Tables) of
                    true -> ok;
                    false -> update_root_schema(C, Logger, BucketName0)
                end,
                io:format("done: ~p~n",
                          [update_table_schema(C, Logger, BucketName0,
                                               static_tabledef())]),

                io:format("Checking schema for normal ssmix records..."),
                BucketName1 = ?SSMIX_BUCKET,
                case lists:member(BucketName1, Tables) of
                    true -> ok;
                    false -> update_root_schema(C, Logger, BucketName1)
                end,
                io:format("done: ~p~n",
                          [update_table_schema(C, Logger, BucketName1,
                                               normal_tabledef())]),

                io:format("Checking schema for rezept..."),
                BucketName2 = ?RECEPT_BUCKET,
                case lists:member(BucketName2, Tables) of
                    true -> ok;
                    false -> update_root_schema(C, Logger ,BucketName2)
                end,
                io:format("done: ~p~n",
                          [update_table_schema(C, Logger, BucketName2,
                                               recept_tabledef())]),
                ok;

            Error ->
                io:format("~p: Cluster is not set up ready for Prestodb.", [Error]),
                error
        end

    after
            meddatum_console:teardown(Context)
    end.


%% ======================== internal functions =======================

%% encodes all atom under this 'map'
atom_to_binary({Obj}) ->
    {lists:map(fun({Key,Val}) ->
                       {klib:maybe_a2b(Key), klib:maybe_a2b(Val)}
               end, Obj)}.

get_tables(C, Logger, _SchemaName = BucketType) ->
    Bucket = {BucketType, ?PRESTO_SCHEMA_BUCKET},
    case riakc_pb_socket:get(C, Bucket, ?PRESTO_SCHEMA_KEY)of
        {ok, RiakObj} ->
            {_, Json} = hd(riakc_obj:get_contents(RiakObj)),
            {List} = jsone:decode(Json),
            Tables = proplists:get_value(<<"tables">>, List),
            treehugger:log(Logger, debug, "Tables in schema: ~p", [Tables]),
            {ok, Tables};
        {error, notfound} ->
            treehugger:log(Logger, error,
                           "No schema data is set up at bucket_type ~p.",
                           [?BUCKET_TYPE]),
            throw(no_tables);

        Error ->
            treehugger:log(Logger, error, "~p:~p ~p", [?FILE, ?LINE, Error]),
            throw(Error)
    end.

update_root_schema(C, Logger, _TableName = Bucket) ->
    B = {?BUCKET_TYPE, ?PRESTO_SCHEMA_BUCKET},
    {ok, RiakObj0} = riakc_pb_socket:get(C, B, ?PRESTO_SCHEMA_KEY),

    {_, Json} = hd(riakc_obj:get_contents(RiakObj0)),
    {List} = jsone:decode(Json),
    Tables0 = proplists:get_value(<<"tables">>, List),
    Comments = proplists:get_value(<<"comments">>, List),
    treehugger:log(Logger, info, "Existing tables: ~p", [Tables0]),
    Tables = [Bucket|Tables0],

    NewJson = jsone:encode({[{<<"tables">>, Tables}, {<<"comments">>, Comments}]}),
    RiakObj = riakc_obj:update_value(RiakObj0, NewJson),

    ok = riakc_pb_socket:put(C, RiakObj),
    J = riakc_obj:get_update_value(RiakObj),
    treehugger:log(Logger, info, "new default schema updated: ~s.", [J]).

update_table_schema(C, Logger, _TableName = Bucket, TableDef) ->
    B = {?BUCKET_TYPE, ?PRESTO_SCHEMA_BUCKET},
    RiakObj = case riakc_pb_socket:get(C, B, Bucket) of
                  {ok, RiakObj0} ->
                      riakc_obj:update_value(RiakObj0, TableDef);
                  {error, notfound} ->
                      riakc_obj:new(B, Bucket, TableDef, "application/json")
              end,

    ok = riakc_pb_socket:put(C, RiakObj),
    treehugger:log(Logger, info, "new default schema updated.", []).

get_tabledef(C, TableName) ->
    SchemaName = ?BUCKET_TYPE,
    {ok, RiakObj} = riakc_pb_socket:get(C, {SchemaName, ?PRESTO_SCHEMA_BUCKET}, TableName),
    {_, Json} = hd(riakc_obj:get_contents(RiakObj)),
    Json.
