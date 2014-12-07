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
    jsone:encode(SSMIXTable0).

normal_tabledef(HospitalID) ->
    {_BType0, BucketName1} = hl7:bucket_from_hospital_id(HospitalID),
    SSMIXTable1 = {[
                    {<<"name">>, BucketName1},
                    {<<"columns">>, [atom_to_binary({Col})||Col<-hl7:columns()]}
                   ]},
    jsone:encode(SSMIXTable1).

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
    io:format("Normal ssmix table: ~s~n", [normal_tabledef(HospitalID)]),
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
        io:format("~p~n", [Tabledef0 =:= Tabledef1]),

        io:format("Checking schema for normal ssmix records on hospital ~s...", [HospitalID]),
        {_BType0, BucketName1} = hl7:bucket_from_hospital_id(HospitalID),
        io:format("~p, ", [lists:member(BucketName1, Tables)]),
        Tabledef00 = normal_tabledef(HospitalID),
        Tabledef01 = get_tabledef(C, BucketName1),
        io:format("~p~n", [Tabledef00 =:= Tabledef01]),

        io:format("Checking schema for rezept on hospital ~s...", [HospitalID]),
        {_BType, BucketName2} = rezept:bucket_from_hospital_id(HospitalID),
        io:format("~p, ", [lists:member(BucketName2, Tables)]),
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
        case get_tables(C, Logger, ?BUCKET_TYPE) of
            {ok, Tables} ->

                io:format("Updating schema for static ssmix records on hospital ~s...", [HospitalID]),
                {_BType0, BucketName0} = hl7:static_bucket_from_hospital_id(HospitalID),
                case lists:member(BucketName0, Tables) of
                    true -> ok;
                    false -> update_root_schema(C, Logger, BucketName0)
                end,
                io:format("done: ~p~n", [update_table_schema(C, Logger, BucketName0, static_tabledef(HospitalID))]),

                io:format("Checking schema for normal ssmix records on hospital ~s...", [HospitalID]),
                {_BType0, BucketName1} = hl7:bucket_from_hospital_id(HospitalID),
                case lists:member(BucketName1, Tables) of
                    true -> ok;
                    false -> update_root_schema(C, Logger, BucketName1)
                end,
                io:format("done: ~p~n", [update_table_schema(C, Logger, BucketName1, normal_tabledef(HospitalID))]),

                io:format("Checking schema for rezept on hospital ~s...", [HospitalID]),
                {_BType, BucketName2} = rezept:bucket_from_hospital_id(HospitalID),
                case lists:member(BucketName2, Tables) of
                    true -> ok;
                    false -> update_root_schema(C, Logger ,BucketName2)
                end,
                io:format("done: ~p~n", [update_table_schema(C, Logger, BucketName2, recept_tabledef(HospitalID))]),
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
