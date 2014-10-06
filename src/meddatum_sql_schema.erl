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
        %% master schema is in <<"__presto_schema">> , <<"default">>
        %% table definition is in <<"__presto_schema">>, <<"default.tablename">>
        {ok, Buckets} = riakc_pb_socket:list_buckets(C, <<"md">>),
        check_default_schema(C, Logger, Buckets),
        lists:foreach(fun(Bucket) -> check_table(C, Logger, Bucket) end, Buckets)
    after
            meddatum_console:teardown(Context)
    end.

setup() ->
    {ok, #context{logger=Logger,
                  riakc=C} = Context} = meddatum_console:setup(),
    try
        maybe_create_default_schema(C, Logger),
        {ok, Buckets} = riakc_pb_socket:list_buckets(C, <<"md">>),
        treehugger:log(Logger, info, "all buckets: ~p", [Buckets]),
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
-define(PRESTO_SCHEMA_KEY, <<"default">>).

check_default_schema(C, Logger, Buckets0) ->
    {ok, RiakObj} = riakc_pb_socket:get(C, ?PRESTO_SCHEMA_BUCKET, ?PRESTO_SCHEMA_KEY),
    {_, Json} = hd(riakc_obj:get_contents(RiakObj)),
    {[{<<"tables">>, {Tables}}]} = jsone:decode(Json),
    TableList = lists:sort([Table || {Table,_} <- Tables]),
    Buckets = lists:sort(Buckets0),
    treehugger:log(Logger, info, "Table defintion: ~p", [TableList]),
    treehugger:log(Logger, info, "Bucket found: ~p", [Buckets]),
    Buckets = TableList.

maybe_update_default_schema(C, Logger, Bucket) ->
    {ok, RiakObj0} = riakc_pb_socket:get(C, ?PRESTO_SCHEMA_BUCKET, ?PRESTO_SCHEMA_KEY),
    {_, Json} = hd(riakc_obj:get_contents(RiakObj0)),
    {[{<<"tables">>, {Tables0}}]} = jsone:decode(Json),
    treehugger:log(Logger, info, "Existing tables: ~p", [Tables0]),
    Tables = case lists:keymember(Bucket, 1, Tables0) of
                 true -> Tables0;
                 _ -> [{Bucket, []}|Tables0]
             end,

    NewJson = jsone:encode({[{<<"tables">>, {Tables}}]}),
    RiakObj = riakc_obj:update_value(RiakObj0, NewJson),
    ok = riakc_pb_socket:put(C, RiakObj),
    treehugger:log(Logger, info, "new default schema updated: ~s.", [NewJson]).

maybe_create_default_schema(C, Logger) ->
    case riakc_pb_socket:get(C, ?PRESTO_SCHEMA_BUCKET, ?PRESTO_SCHEMA_KEY) of
        {ok, _RiakObj} ->
            treehugger:log(Logger, info, "presto default schema already exists.", []);
        {error, notfound} ->
            treehugger:log(Logger, info, "presto default schema not found: creating", []),
            RiakObj = riakc_obj:new(?PRESTO_SCHEMA_BUCKET, ?PRESTO_SCHEMA_KEY,
                                   <<"{\"tables\":{}}">>, "application/json"),
            ok = riakc_pb_socket:put(C, RiakObj),
            treehugger:log(Logger, info, "new default schema created.", [])
    end.

check_table(C, Logger, Bucket) ->
    Key = <<(?PRESTO_SCHEMA_KEY)/binary, ".", Bucket/binary>>,
    case riakc_pb_socket:get(C, ?PRESTO_SCHEMA_BUCKET, Key) of
        {ok, RiakObj} ->
            {_, Json} = hd(riakc_obj:get_contents(RiakObj)),
            Table = jsone:decode(Json),
            treehugger:log(Logger, info, "Definition of table '~s': ~p", [Bucket, Table]);
        {error, notfound} ->
            treehugger:log(Logger, error, "no table definition found for table ~s", [Bucket])
    end.

maybe_create_table(C, Logger, Bucket) ->
    case Bucket of
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

maybe_create_table(C, Logger, Bucket, Table) ->
    maybe_update_default_schema(C, Logger, Bucket),
    JSON = jsone:encode(Table),
    Key = <<(?PRESTO_SCHEMA_KEY)/binary, ".", Bucket/binary>>,
    RiakObj = riakc_obj:new(?PRESTO_SCHEMA_BUCKET, Key,
                            JSON, "application/json"),
    ok = riakc_pb_socket:put(C, RiakObj),
    treehugger:log(Logger, info, "Table ~s created: ~s", [Bucket, JSON]).
