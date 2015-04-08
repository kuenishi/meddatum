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

-module(md_record).

-include("meddatum.hrl").
-export([bucket2hospital_id/1,
         put_json/4,
         mark_set_as_done/3, check_is_set_done/3]).

%% note that the file is usually *NOT* JSON.
-callback from_file(filename:filename(), list(), pid()) -> {ok, [_]}.
-callback from_file(filename:filename(), list(), pid(), PostProcessor::fun()) -> {ok, [_]}.

-callback to_json(_) ->  {ok, JSON::binary()}.
-callback from_json(JSON::binary()) -> _.
-callback key(_) -> binary().
-callback bucket(_) -> binary().
-callback make_2i_list(_) -> [{binary(), binary()|integer()}].
-callback patient_id(_) -> binary().
-callback hospital_id(_) -> binary().
-callback columns() -> JSON::binary().

-callback mark_set_as_done(RiakClient::pid(), Identifier::term()) ->
    ok | {error, term()}.
-callback check_is_set_done(RiakClient::pid(), Identifier::term()) ->
    boolean().

-spec put_json(pid(), _::any(), module(), pid()) -> ok.
put_json(C, Msg, Mod, Logger) when is_atom(Mod) ->
    Key = Mod:key(Msg),
    case Mod:to_json(Msg) of
        {ok, Data} when is_binary(Data) ->
            Bucket = Mod:bucket(Msg),
            RiakObj0 = meddatum:maybe_new_ro(C, Bucket, Key, Data),
            Indexes = Mod:make_2i_list(Msg),
            Metadata0 = riakc_obj:get_update_metadata(RiakObj0),
            Metadata = lists:foldl(fun({K,V}, MD0) when is_binary(V) ->
                                           riakc_obj:set_secondary_index(MD0, {{binary_index, K}, [V]});
                                      ({K,V}, MD0) when is_integer(V) ->
                                           riakc_obj:set_secondary_index(MD0, {{integer_index, K}, [V]})
                                   end, Metadata0, Indexes),
            RiakObj = riakc_obj:update_metadata(RiakObj0, Metadata),
            %%_ = lager:debug("inserting: ~p~n", [Key]),
            treehugger:log(Logger, debug, "inserting ~p", [{Bucket,Key}]),
            case riakc_pb_socket:put(C, RiakObj) of
                ok -> ok;
                Error -> treehugger:log(error, Logger, "error inserting ~p: ~p", [Key, Error])
            end;
        E ->
            treehugger:log(Logger, error, "can't encode record (~p): ~p", [E, Msg]),
            E
    end.

bucket2hospital_id({_, Bucket}) -> bucket2hospital_id(Bucket);
bucket2hospital_id(Bucket) when is_binary(Bucket) ->
    case Bucket of
        <<"ssmix:", HospitalID/binary>> ->
            {ssmix, HospitalID};
        <<"ssmix-patients:", HospitalID/binary>> ->
            {ssmix_patients, HospitalID};
        <<"recept:", HospitalID/binary>> ->
            {recept, HospitalID}
    end.

-spec mark_set_as_done(pid(), module(), term()) -> ok | {error, term()}.
mark_set_as_done(C, Module, Identifier) ->
    Module:mark_set_as_done(C, Identifier).

-spec check_is_set_done(pid(), module(), term()) -> boolean().
check_is_set_done(C, Module, Identifier) ->
    Module:check_is_set_done(C, Identifier).
