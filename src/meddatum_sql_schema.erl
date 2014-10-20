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

-export([create/0, check/0, setup/0]).

create() ->
    SSMIXTable = {[
                   {<<"name">>, <<"<SSMIX table name>">>},
                   {<<"columns">>, [atom_to_binary({Col})||Col<-hl7:columns()]}
                  ]},
    ReceptTable = {[
                    {<<"name">>, <<"<Recept table name>">>},
                    {<<"columns">>, [atom_to_binary({C})||C<-rezept:columns()]}
                   ]},
    io:format("~s~n", [jsone:encode([SSMIXTable, ReceptTable])]).

check() ->
    {ok, #context{logger=Logger,
                  riakc=C} = Context} = meddatum_console:setup(),
    try
        %% check presto-riak enabled or not
        %% {ok, Props} = riakc_pb_socket:get_bucket_type(C, ?BUCKET_TYPE),
        %% case proplists:get_value(presto_enabled, Props) of
        %%     true -> ok;
        %%     _ -> treehugger:log(Logger, error, "presto is not enabled at bucket type ~p.",
        %%                         [?BUCKET_TYPE]),
        %%          halt(1)
        %% end,
        treehugger:log(Logger, info, "checking schema ~p...", [?BUCKET_TYPE]),
        %% master schema is in {TYPE, <<"__presto_schema">>} , <<"default">>
        %% table definition is in <<"__presto_schema">>, <<"default.tablename">>
        {ok, Buckets} = riakc_pb_socket:list_buckets(C, ?BUCKET_TYPE),
        check_schema(C, Logger, ?BUCKET_TYPE, Buckets),
        lists:foreach(fun(Bucket) -> check_table(C, Logger, ?BUCKET_TYPE, Bucket) end, Buckets)
    after
            meddatum_console:teardown(Context)
    end.

setup() ->
    {ok, #context{logger=Logger,
                  riakc=C} = Context} = meddatum_console:setup(),
    try
        %% maybe_create_schema(C, Logger, ?BUCKET_TYPE),
        {ok, Buckets} = riakc_pb_socket:list_buckets(C, ?BUCKET_TYPE),
        treehugger:log(Logger, info, "all buckets in ~p: ~p", [?BUCKET_TYPE, Buckets]),
        lists:foreach(fun(Bucket) -> maybe_create_table(C, Logger, Bucket) end, Buckets)
    after
            meddatum_console:teardown(Context)
    end.


%% ======================== internal functions =======================

%% encodes all atom under this 'map'
atom_to_binary({Obj}) ->
    {lists:map(fun({Key,Val}) ->
                       {klib:maybe_a2b(Key), klib:maybe_a2b(Val)}
               end, Obj)}.

-define(PRESTO_SCHEMA_BUCKET, <<"__presto_schema">>).
-define(PRESTO_SCHEMA_KEY, <<"__schema">>).

check_schema(C, Logger, _SchemaName = BucketType, Buckets0) ->
    Bucket = {BucketType, ?PRESTO_SCHEMA_BUCKET},
    {ok, RiakObj} = riakc_pb_socket:get(C, Bucket, ?PRESTO_SCHEMA_KEY),
    {_, Json} = hd(riakc_obj:get_contents(RiakObj)),
    {[{<<"tables">>, {Tables}}]} = jsone:decode(Json),
    treehugger:log(Logger, debug, "Tables in schema: ~p", [Tables]),
    TableList = lists:sort([Table || {Table,_} <- Tables]),
    Buckets = lists:sort(Buckets0),
    treehugger:log(Logger, info, "Table defintion: ~p", [TableList]),
    treehugger:log(Logger, info, "Buckets found: ~p", [Buckets]),
    BucketsSet = sets:from_list(Buckets),
    TablesSet = sets:from_list(TableList ++ [?PRESTO_SCHEMA_BUCKET]),
    case sets:is_subset(TablesSet, BucketsSet) andalso sets:is_subset(BucketsSet, TablesSet) of
        true -> treehugger:log(Logger, info, "Buckets matched", []);
        false -> treehugger:log(Logger, error, "Buckets and tables didn't match!", [])
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

check_table(C, Logger, SchemaName, TableName) ->
    case riakc_pb_socket:get(C, {SchemaName, ?PRESTO_SCHEMA_BUCKET}, TableName) of
        {ok, RiakObj} ->
            {_, Json} = hd(riakc_obj:get_contents(RiakObj)),
            Table = jsone:decode(Json),
            treehugger:log(Logger, info, "Definition of table '~s': ~p",
                           [TableName, Table]);
        {error, notfound} ->
            treehugger:log(Logger, error, "no table definition found for table ~s",
                           [TableName])
    end.

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
