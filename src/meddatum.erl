%%
%% Copyright (C) 2013-2014 UENISHI Kota
%%
%%    Licensed under the Apache License, Version 2.0 (the "License");
%%    you may not use this file except in compliance with the License.
%%    You may obtain a copy of the License at
%%
%%        http://www.apache.org/licenses/LICENSE-2.0
%%
%%    Unless required by applicable law or agreed to in writing, software
%%    distributed under the License is distributed on an "AS IS" BASIS,
%%    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%    See the License for the specific language governing permissions and
%%    limitations under the License.
%%

-module(meddatum).

-export([main/1, help/0, maybe_new_ro/4,
         check_setup/0,
         setup/2]).

-include_lib("eunit/include/eunit.hrl").
-include("meddatum.hrl").

main(["create-config"]) -> meddatum_console:create_config();
main(["check-config"]) ->  meddatum_console:check_config();
main(["setup-riak"]) ->    meddatum_console:setup_riak();
main(["import-ssmix"|Args]) ->  meddatum_console:import_ssmix(Args);
main(["import-recept"|Args]) -> meddatum_console:import_recept(Args);
main(["import-dpcs", "-f"|Args]) -> meddatum_console:import_dpcs(Args, true);
main(["import-dpcs"|Args]) -> meddatum_console:import_dpcs(Args, false);
main(["parse-ssmix"|Args]) ->   meddatum_console:parse_ssmix(Args);
main(["parse-recept"|Args]) ->  meddatum_console:parse_recept(Args);
main(["parse-dpcs"|Args]) ->  meddatum_console:parse_dpcs(Args);
main(["delete-all-ssmix"|Args]) -> meddatum_console:delete_all_ssmix(Args);
main(["delete-recept"|Args]) ->    meddatum_console:delete_recept(Args);
main(["search"|Args]) ->    meddatum_console:search(Args);
main(["show-schema", "ssmix-static"]) ->
    meddatum_sql_schema:create(static);
main(["show-schema", "ssmix"]) ->
    meddatum_sql_schema:create(ssmix);
main(["show-schema", "recept"]) ->
    meddatum_sql_schema:create(recept);
main(["show-schema", "dpcs"|Opts]) ->
    meddatum_sql_schema:create(dpcs, Opts);
main(["check-schema"]) ->
    meddatum_sql_schema:check();
main(["setup-schema"]) ->
    meddatum_sql_schema:setup();
main(["help"]) -> help();
main(_) -> help().

help() ->
    io:format("usage:~n"
              "meddatum create-config (configuration file will be created at ~~/.meddatum~n"
              "meddatum check-config (checks configuration file ~~/.meddatum)~n"
              "meddatum setup-riak   (setup Riak Search for healthb)~n"
              "meddatum import-ssmix <hospital-id> <path/to/directory>~n"
              "meddatum import-recept [dpc|med] <path/to/file>~n"
              "meddatum import-dpcs [-f] <dir> <hospital-id> <yymm> [-Dn|-EFn|-EFg|-FF1|-FF4 <path/to/file>]~n"
              "meddatum parse-ssmix <ssmix-file or ssmix-dir> (test parsing ssmix file)~n"
              "meddatum parse-recept [dpc|med] <recept-file> (test parsing recept file)~n"
              "meddatum parse-dpcs <dir> <hospital-id> <yymm> [-Dn|-EFn|-EFg|-FF1|-FF4 <path/to/file>]~n"
              "meddatum delete-all-ssmix <hospital-id>~n"
              "meddatum delete-recept <recept-file>~n"
              "meddatum search <keyword> (prints all keys matched)~n"
              "meddatum [help]~n"
              "experimental:~n"
              "meddatum show-schema [ssmix-static|ssmix|recept|dpcs [efndn|efg|ff]]~n"
              "meddatum ckeck-schema (checks tabledef of ssmix, recept in Riak)~n"
              "meddatum setup-schema (creates tabledef on ssmix, recept)~n"
             ).

-define(RESULT(N, X), io:format("~s: ~p~n", [(N), (X)])).

setup(Host, Port) ->
    {ok, C} = riakc_pb_socket:start_link(Host, Port),

    %% maybe setup schema
    SchemaName = ?SCHEMA_NAME,
    case riakc_pb_socket:get_search_schema(C, SchemaName) of
        {ok, Schema} ->
            ?RESULT("schema", Schema);
        {error, E} ->
            ?RESULT("schema not found", E),
            Binary = meddatum_catch_all_schema:binary(),
            %% io:format("~s", [Binary]),
            ok = riakc_pb_socket:create_search_schema(C, SchemaName, Binary)
    end,

    {ok, Indexes} = riakc_pb_socket:list_search_indexes(C),
    ?RESULT("indexes", Indexes),

    case riakc_pb_socket:get_search_index(C, ?INDEX_NAME) of
        {ok, Result} -> ?RESULT("get search index", Result);
        {error, _} ->
            ?RESULT("create_search_index",
                    riakc_pb_socket:create_search_index(C, ?INDEX_NAME, SchemaName, []))
    end,

    case riakc_pb_socket:get_bucket_type(C, ?BUCKET_TYPE) of
        {error, Reason} ->
            ?RESULT("get_bucket_type failed", Reason);
        {ok, Props} ->
            case proplists:get_value(search_index, Props) of
                ?INDEX_NAME ->
                    ?RESULT("bucket type already registered", ?INDEX_NAME);
                undefined ->
                    NewProps = [{search_index, ?INDEX_NAME}|Props],
                    R0 = riakc_pb_socket:set_bucket_type(C, ?BUCKET_TYPE, NewProps),
                    ?RESULT("bucket type indexing is set", R0)
            end
    end,

    {B, K} = {{?BUCKET_TYPE, <<"b">>}, <<"k">>},
    Obj0 = maybe_new_ro(C, B, K, <<"{\"test\":1}">>),
    ?RESULT("get a test data", riakc_obj:get_contents(Obj0)),

    ?RESULT("put a test data", riakc_pb_socket:put(C, Obj0)),
    timer:sleep(100),
    SO = [], %%[{rows,1024},{fl,<<"foobar">>}],
    {ok, SearchResult} = riakc_pb_socket:search(C, ?INDEX_NAME, <<"*:*">>, SO),
    ?RESULT("try a search", SearchResult),

    ?RESULT("delete test data", riakc_pb_socket:delete(C, B, K)),
    %% ?RESULT("delete search index", riakc_pb_socket:delete_search_index(C, <<"md_index">>)),
    ?RESULT("get k/v", riakc_pb_socket:get(C, B, K)),

    ok = riakc_pb_socket:stop(C).

maybe_new_ro(C, B, K, D) ->
    case riakc_pb_socket:get(C, B, K) of
        {ok, RiakObj0} ->
            riakc_obj:update_value(RiakObj0, D, ?APPLICATION_JSON);
        {error, _E} ->
            riakc_obj:new(B, K, D, ?APPLICATION_JSON)
    end.

-define(msg(Name), io:format("checking ~s ...", [Name])).
-define(res(Argv), io:format("ok: ~p.~n", [Argv])).

check_setup() ->
    {ok, {Host, Port}} = meddatum_config:get_riak(),
    {ok, Pid} = riakc_pb_socket:start_link(Host, Port),
    ?msg("search schema name"),
    {ok, Schema} = riakc_pb_socket:get_search_schema(Pid, ?SCHEMA_NAME),
    ?SCHEMA_NAME = proplists:get_value(name, Schema),
    ?res(?SCHEMA_NAME),
    SchemaBin = meddatum_catch_all_schema:binary(),
    ?msg("search schema content"),
    SchemaBin = proplists:get_value(content, Schema),
    ?res(match),
    {ok, Index} = riakc_pb_socket:get_search_index(Pid, ?INDEX_NAME),
    ?msg("search index created"),
    ?INDEX_NAME = proplists:get_value(index, Index),
    ?res(yes),
    %% <<"_yz_default">> = proplists:get_value(schema, Index),
    ?msg("valid schema for index"),
    ?SCHEMA_NAME = proplists:get_value(schema, Index),
    ?res(proplists:get_value(schema, Index)),
    {ok, Props} = riakc_pb_socket:get_bucket_type(Pid, ?BUCKET_TYPE),
    ?msg("valid index for bucket type"),
    ?INDEX_NAME = proplists:get_value(search_index, Props),
    ?res(?INDEX_NAME),
    ?msg("presto enabled"),
    ?res(proplists:get_value(presto_enabled, Props)),
    ok = riakc_pb_socket:stop(Pid),
    true.
