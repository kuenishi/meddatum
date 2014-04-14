%%
%% Copyright (C) 2013-2013 UENISHI Kota
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

-export([true_bucket_name/1, maybe_new_ro/5,
         ssmix_bucket/1, recept_bucket/1, ssmix_patients_bucket/1,
         setup/2]).

-include_lib("eunit/include/eunit.hrl").
-include("meddatum.hrl").

true_bucket_name(Bucket0) ->
    case meddatum_config:use_bucket_types() of
        true -> {?BUCKET_TYPE, Bucket0};
        false -> Bucket0
    end.

%% @private
ssmix_bucket(HospitalID) when is_binary(HospitalID) ->
    io:format("~p~n", [HospitalID]),
    Bucket = <<?SSMIX_BUCKET/binary, ?BUCKET_NAME_SEPARATOR/binary, HospitalID/binary>>,
    meddatum:true_bucket_name(Bucket);
ssmix_bucket(HospitalID) ->
    ssmix_bucket(iolist_to_binary(HospitalID)).

%% @private
recept_bucket(HospitalID) when is_binary(HospitalID) ->
    Bucket = <<?RECEPT_BUCKET/binary, ?BUCKET_NAME_SEPARATOR/binary, HospitalID/binary>>,
    meddatum:true_bucket_name(Bucket);
recept_bucket(HospitalID) ->
    recept_bucket(iolist_to_binary(HospitalID)).

%% @private
ssmix_patients_bucket(HospitalID) when is_binary(HospitalID) ->
    Bucket = <<?SSMIX_PATIENTS_BUCKET/binary, ?BUCKET_NAME_SEPARATOR/binary, HospitalID/binary>>,
    meddatum:true_bucket_name(Bucket);
ssmix_patients_bucket(HospitalID) ->
    ssmix_patients_bucket(iolist_to_binary(HospitalID)).

%% Maybe new Riak Object
maybe_new_ro(Client, Bucket, Key, Data, ContentType) ->
    case riakc_pb_socket:get(Client, Bucket, Key) of
        {ok, RiakObj0} ->
            case riakc_obj:value_count(RiakObj0) of
                1 ->
                    riakc_obj:update_value(RiakObj0, Data, ContentType);
                N when N > 1 ->
                    RiakObj = riakc_obj:new(Bucket, Key,
                                            Data, ContentType),
                    riakc_obj:set_vclock(RiakObj, riakc_obj:vclock(RiakObj0))
            end;
        {error, _E} ->
            _ = lager:debug("inserting ~p/~p: ~p~n", [Bucket, Key, _E]),
            riakc_obj:new(Bucket, Key, Data, ContentType)
    end.

-define(RESULT(N, X), io:format("~s: ~p~n", [(N), (X)])).

setup(Host, Port) ->
    {ok, C} = riakc_pb_socket:start_link(Host, Port),
    {ok, Indexes} = riakc_pb_socket:list_search_indexes(C),
    ?RESULT("indexes", Indexes),

    case riakc_pb_socket:get_search_index(C, ?INDEX_NAME) of
        {ok, Result} -> ?RESULT("get search index", Result);
        {error, _} ->
            ?RESULT("create_search_index",
                    riakc_pb_socket:create_search_index(C, ?INDEX_NAME))
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
                    R0 = riakc_pb_socket:set_bucket_type(C, NewProps),
                    ?RESULT("bucket type indexing is set", R0)
            end
    end,

    {B, K} = {{?BUCKET_TYPE, <<"b">>}, <<"k">>},
    Obj0 = meddatum:maybe_new_ro(C, B, K,
                                 <<"{\"test\":1}">>, "application/json"),
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

