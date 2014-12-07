%%%-------------------------------------------------------------------
%%% @author UENISHI Kota <kota@basho.com>
%%% @copyright (C) 2014, UENISHI Kota
%%% @doc
%%%
%%% @end
%%% Created :  5 Oct 2014 by UENISHI Kota <kota@basho.com>
%%%-------------------------------------------------------------------
-module(meddatum_sql_schema).

-include("meddatum.hrl").

-export([create/1, check/1, setup/1]).

%% meta structures are here
-define(PRESTO_SCHEMA_BUCKET, <<"__presto_schema">>).
%% all tables' name should be here
-define(PRESTO_SCHEMA_KEY, <<"__schema">>).

static_tabledef(HospitalID) ->
    {_BType0, BucketName0} = hl7:static_bucket_from_hospital_id(HospitalID),
    SSMIXTable0 = {[
                    {<<"name">>, BucketName0},
                    {<<"columns">>, [atom_to_binary({Col})||Col<-hl7:columns()]}
                   ]},
    jsone:encode([SSMIXTable0]).

normal_tabledef(HospitalID) ->
    {_BType0, BucketName1} = hl7:bucket_from_hospital_id(HospitalID),
    SSMIXTable1 = {[
                    {<<"name">>, BucketName1},
                    {<<"columns">>, [atom_to_binary({Col})||Col<-hl7:columns()]}
                   ]},
    jsone:encode([SSMIXTable1]).

recept_tabledef(HospitalID) ->
    {_BType, BucketName2} = rezept:bucket_from_hospital_id(HospitalID),
    ReceptTable = {[
                    {<<"name">>, BucketName2},
                    {<<"columns">>, [atom_to_binary({C})||C<-rezept:columns()]}
                   ]},
    jsone:encode(ReceptTable).

create(HospitalID0) ->
    HospitalID = list_to_binary(HospitalID0),
    io:format("Static ssmix table:~s~n", [static_tabledef(HospitalID)]),
    io:format("Normal ssmix table: ~s~n", [normal_tabledef([HospitalID])]),
    io:format("Recept table: ~s~n", [recept_tabledef(HospitalID)]).

check(HospitalID0) ->
    HospitalID = list_to_binary(HospitalID0),
    {ok, #context{logger=Logger,
                  riakc=C} = Context} = meddatum_console:setup(),
    try
        %% check presto-riak enabled or not
        {ok, Tables} = get_tables(C, Logger, ?BUCKET_TYPE),

        %% table definition is in {<<"md">>, <<"__presto_schema">>}, <<"tablename">>
        io:format("Checking schema for static ssmix records on hospital ~s...", [HospitalID]),
        {_BType0, BucketName0} = hl7:static_bucket_from_hospital_id(HospitalID),
        io:format("~p, ", [lists:member(BucketName0, Tables)]),
        Tabledef0 = static_tabledef(HospitalID),
        Tabledef1 = get_tabledef(C, BucketName0),
        Io:format("~p~n", [Tabledef0 =:= Tabledef1]),

        io:format("Checking schema for normal ssmix records on hospital ~s...", [HospitalID]),
        {_BType0, BucketName1} = hl7:bucket_from_hospital_id(HospitalID),
        io:format("~p~n", [lists:member(BucketName1, Tables)]),
        Tabledef00 = normal_tabledef(HospitalID),
        Tabledef01 = get_tabledef(C, BucketName1),
        io:format("~p~n", [Tabledef00 =:= Tabledef01]),

        io:format("Checking schema for rezept on hospital ~s...", [HospitalID]),
        {_BType, BucketName2} = rezept:bucket_from_hospital_id(HospitalID),
        io:format("~p~n", [lists:member(BucketName2, Tables)]),
        Tabledef10 = recept_tabledef(HospitalID),
        Tabledef11 = get_tabledef(C, BucketName2),
        io:format("~p~n", [Tabledef10 =:= Tabledef11]),
        ok

    after
            meddatum_console:teardown(Context)
    end.

setup(HospitalID0) ->
    HospitalID = list_to_binary(HospitalID0),
    {ok, #context{logger=Logger,
                  riakc=C} = Context} = meddatum_console:setup(),
    try
        %% check presto-riak enabled or not
        {ok, Tables} = get_tables(C, Logger, ?BUCKET_TYPE),

        %% table definition is in {<<"md">>, <<"__presto_schema">>}, <<"tablename">>
        io:format("Checking schema for static ssmix records on hospital ~s...", [HospitalID]),
        {_BType0, BucketName0} = hl7:static_bucket_from_hospital_id(HospitalID),
        io:format("~p~n", [lists:member(BucketName0, Tables)]),

        io:format("Checking schema for normal ssmix records on hospital ~s...", [HospitalID]),
        {_BType0, BucketName1} = hl7:bucket_from_hospital_id(HospitalID),
        io:format("~p~n", [lists:member(BucketName1, Tables)]),

        io:format("Checking schema for rezept on hospital ~s...", [HospitalID]),
        {_BType, BucketName2} = rezept:bucket_from_hospital_id(HospitalID),
        io:format("~p~n", [lists:member(BucketName2, Tables)]),

        %% Check each table definition here
        error

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
        {error, not_found} ->
            treehugger:log(Logger, error,
                           "No schema data is set up at bucket_type ~p.",
                           [?BUCKET_TYPE]),
            throw(no_tables);

        Error ->
            treehugger:log(Logger, error, "~p:~p ~p", [?FILE, ?LINE, Error]),
            throw(Error)
    end.

maybe_update_schema(C, Logger, _SchemaName = BucketType, _TableName = Bucket) ->
    B = {BucketType, ?PRESTO_SCHEMA_BUCKET},
    RiakObj = case riakc_pb_socket:get(C, B, ?PRESTO_SCHEMA_KEY) of
                  {ok, RiakObj0} ->
                      {_, Json} = hd(riakc_obj:get_contents(RiakObj0)),
                      {[{<<"tables">>, {Tables0}}]} = jsone:decode(Json),
                      treehugger:log(Logger, info, "Existing tables: ~p", [Tables0]),
                      Tables = [{Bucket, []}|Tables0],

                      NewJson = jsone:encode({[{<<"tables">>, {Tables}}]}),
                      riakc_obj:update_value(RiakObj0, NewJson);
                  {error, notfound} ->
                      Tables = [{Bucket, []}],
                      NewJson = jsone:encode({[{<<"tables">>, {Tables}}]}),
                      riakc_obj:new(B, ?PRESTO_SCHEMA_KEY,
                                    NewJson, "application/json")
              end,

    ok = riakc_pb_socket:put(C, RiakObj),
    J = riakc_obj:get_update_value(RiakObj),
    treehugger:log(Logger, info, "new default schema updated: ~s.", [J]).

%% maybe_create_default_schema(C, Logger) ->
%%     case riakc_pb_socket:get(C, ?PRESTO_SCHEMA_BUCKET, ?PRESTO_SCHEMA_KEY) of
%%         {ok, _RiakObj} ->
%%             treehugger:log(Logger, info, "presto default schema already exists.", []);
%%         {error, notfound} ->
%%             treehugger:log(Logger, info, "presto default schema not found: creating", []),
%%             RiakObj = riakc_obj:new(?PRESTO_SCHEMA_BUCKET, ?PRESTO_SCHEMA_KEY,
%%                                    <<"{\"tables\":{}}">>, "application/json"),
%%             ok = riakc_pb_socket:put(C, RiakObj),
%%             treehugger:log(Logger, info, "new default schema created.", [])
%%     end.

get_tabledef(C, TableName) ->
    SchemaName = ?BUCKET_TYPE,
    {ok, RiakObj} = riakc_pb_socket:get(C, {SchemaName, ?PRESTO_SCHEMA_BUCKET}, TableName),
    {_, Json} = hd(riakc_obj:get_contents(RiakObj)),
    Json.

maybe_create_table(C, Logger, Bucket) ->
    case Bucket of
        ?PRESTO_SCHEMA_BUCKET -> ok;
        <<"ssmix:", _/binary>> ->
            maybe_create_ssmix_table(C, Logger, Bucket);
        <<"ssmix-patients:", _/binary>> ->
            maybe_create_ssmix_table(C, Logger, Bucket);
        <<"recept:", _/binary>> ->
            maybe_create_recept_table(C, Logger, Bucket)
    end.

maybe_create_ssmix_table(C, Logger, Bucket) ->
    SSMIXTable = {[
                   {<<"name">>, Bucket},
                   {<<"columns">>, [atom_to_binary({Col})||Col<-hl7:columns()]}
                  ]},
    maybe_create_table(C, Logger, Bucket, SSMIXTable).

maybe_create_recept_table(C, Logger, Bucket) ->
    ReceptTable = {[
                   {<<"name">>, Bucket},
                   {<<"columns">>, [atom_to_binary({Col})||Col<-rezept:columns()]}
                  ]},
    maybe_create_table(C, Logger, Bucket, ReceptTable).

maybe_create_table(C, Logger, TableName, Table) ->
    B = {?BUCKET_TYPE, ?PRESTO_SCHEMA_BUCKET},
    maybe_update_schema(C, Logger, ?BUCKET_TYPE, TableName),
    JSON = jsone:encode(Table),
    RiakObj = riakc_obj:new(B, TableName, JSON, "application/json"),
    ok = riakc_pb_socket:put(C, RiakObj),
    treehugger:log(Logger, info, "Table ~s created: ~s",
                   [TableName, JSON]).
