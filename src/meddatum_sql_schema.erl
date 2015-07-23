%%% @doc
%%%  meddatum json-schema for presto-riak
%%% @end
-module(meddatum_sql_schema).

-include("meddatum.hrl").

-export([create/1, create/2, check/0, setup/0]).

%% meta structures are here
-define(PRESTO_SCHEMA_BUCKET, <<"__presto_schema">>).
%% all tables' name should be here
-define(PRESTO_SCHEMA_KEY, <<"__schema">>).

static_tabledef() ->
    SSMIXTable0 = {[
                    {<<"name">>, ?SSMIX_PATIENTS_BUCKET},
                    {<<"columns">>, [atom_to_binary({Col})||Col<-hl7:columns()]},
                    {<<"comment">>, <<"SSMIX2 Static data">>}
                   ] ++
                       case hl7:subtables() of
                           [] -> [];
                           Subtables -> [{subtables, Subtables}]
                       end
                  },
    jsone:encode(SSMIXTable0, [native_utf8]).

normal_tabledef() ->
    SSMIXTable1 = {[
                    {<<"name">>, ?SSMIX_BUCKET},
                    {<<"columns">>, [atom_to_binary({Col})||Col<-hl7:columns()]},
                    {<<"comment">>, <<"SSMIX2 Normal data">>}
                   ] ++
                       case hl7:subtables() of
                           [] -> [];
                           Subtables -> [{subtables, Subtables}]
                       end
                  },
    jsone:encode(SSMIXTable1, [native_utf8]).

recept_tabledef() ->
    ReceptTable = {[
                    {<<"name">>, ?RECEPT_BUCKET},
                    {<<"columns">>, [atom_to_binary({C})||C<-rezept:columns()]},
                    {<<"comment">>, <<"E-Recept data">>}
                   ] ++
                       case rezept:subtables() of
                           [] -> [];
                           Subtables -> [{subtables, Subtables}]
                       end},
    jsone:encode(ReceptTable, [native_utf8]).

dpcs_tabledef(RecordType) when is_atom(RecordType) ->
    %% Bucket names: dpcs:efndn / dpcs:efg / dpcs:ff
    Bin = atom_to_binary(RecordType, latin1),
    BucketName = <<"dpcs:", Bin/binary>>,
    Subtables = case RecordType of
                    ff ->
                        dpcs:subtables(ff4) ++ dpcs_ff1:subtables();
                    efndn ->
                        dpcs:subtables(efndn);
                    efg ->
                        dpcs:subtables(efg)
                end,
    Table = {[{name, BucketName},
              {columns, dpcs:columns()},
              {comment, <<"DPC Survey Data">>},
              {subtables, Subtables}]},
    jsone:encode(Table, [native_utf8]).

create(Type) ->
    ok = io:setopts([{encoding,utf8}]),
    Text= case Type of
              static -> static_tabledef();
              ssmix ->  normal_tabledef();
              recept -> recept_tabledef()
          end,
    io:format("~ts~n", [Text]).

create(dpcs, [RecordType]) ->
    ok = io:setopts([{encoding,utf8}]),
    Text = case RecordType of
               "efndn" -> dpcs_tabledef(efndn);
               "efg" ->  dpcs_tabledef(efg);
               "ff" -> dpcs_tabledef(ff)
           end,
    io:format("~ts~n", [Text]).

check() ->
    {ok, #context{logger=Logger,
                  riakc=C} = Context} = meddatum_console:setup(),
    try
        %% check presto-riak enabled or not
        {ok, Tables} = get_tables(C, Logger, ?BUCKET_TYPE),

        %% table definition is in {<<"md">>, <<"__presto_schema">>}, <<"tablen%% ame">>
        Results =
            [maybe_check_schema(C, Tables, BucketName, Tabledef, Name)
             || {BucketName, Tabledef, Name}
                    <- [{?SSMIX_PATIENTS_BUCKET, static_tabledef(), "static ssmix"},
                        {?SSMIX_BUCKET, normal_tabledef(), "normal ssmix"},
                        {?RECEPT_BUCKET, recept_tabledef(), "recept"},
                        {<<"dpcs:efndn">>, dpcs_tabledef(efndn), "EFn and Dn"},
                        {<<"dpcs:efg">>, dpcs_tabledef(efg), "EFg"},
                        {<<"dpcs:ff">>, dpcs_tabledef(ff), "FF1 and FF4"}
]],
        lists:all(fun({L, R}) -> L andalso R end, Results)
    after
            meddatum_console:teardown(Context)
    end.

maybe_check_schema(C, Tables, BucketName, Tabledef, Name) ->
    io:format("Checking schema for ~s ...", [Name]),
    InMeta = lists:member(BucketName, Tables),
    %% io:format("Bucket ~s in Root schema: ~p~n", [BucketName, InMeta]),
    TabledefInRiak = get_tabledef(C, BucketName),
    Match = TabledefInRiak =:= Tabledef,
    %% io:format("Tabledef equals: ~p~n", [Match]),
    io:format("~p, ~p~n", [Match, InMeta]),
    {Match, InMeta}.

setup() ->
    {ok, #context{logger=Logger,
                  riakc=C} = Context} = meddatum_console:setup(),
    try
        %% check presto-riak enabled or not
        case get_tables(C, Logger, ?BUCKET_TYPE) of
            {ok, Tables} ->

                maybe_update_schemas(C, Logger, Tables, ?SSMIX_PATIENTS_BUCKET,
                                     static_tabledef(), "static ssmix"),
                maybe_update_schemas(C, Logger, Tables, ?SSMIX_BUCKET,
                                     normal_tabledef(), "normal ssmix"),
                maybe_update_schemas(C, Logger, Tables, ?RECEPT_BUCKET,
                                     recept_tabledef(), "recept"),

                %% DPCS
                maybe_update_schemas(C, Logger, Tables, <<"dpcs:efndn">>,
                                     dpcs_tabledef(efndn), "EFn and Dn"),
                maybe_update_schemas(C, Logger, Tables, <<"dpcs:efg">>,
                                     dpcs_tabledef(efg), "EFg"),
                maybe_update_schemas(C, Logger, Tables, <<"dpcs:ff">>,
                                     dpcs_tabledef(ff), "FF1 and FF4"),

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
        {error, notfound} = E->
            treehugger:log(Logger, error,
                           "No schema data is set up at bucket_type ~p.",
                           [?BUCKET_TYPE]),
            E;

        Error ->
            treehugger:log(Logger, error, "~p:~p ~p", [?FILE, ?LINE, Error]),
            Error
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


maybe_update_schemas(C, Logger, Tables, BucketName, Tabledef, Name) ->
    io:format("Updating schema for ~s records ...", [Name]),
    case lists:member(BucketName, Tables) of
        true -> ok;
        false -> update_root_schema(C, Logger, BucketName)
    end,
    update_table_schema(C, Logger, BucketName, Tabledef).

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
