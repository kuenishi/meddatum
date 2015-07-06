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

-module(ssmix_importer).
-export([walk/3,
         delete_all/2]).

-include("hl7.hrl").
-include("meddatum.hrl").
-include_lib("eunit/include/eunit.hrl").

walk(Path, HospitalID, Ctx) when is_list(HospitalID) ->
    walk(Path, unicode:characters_to_binary(HospitalID), Ctx);
walk(Path, HospitalID, Ctx) when is_binary(HospitalID) ->
    F = fun(File, Acc0) ->
                case process_file(File, HospitalID, Ctx) of
                    ok -> Acc0;
                    {error,_} when is_list(Acc0) ->
                        [File|Acc0];
                    {error, {bad_suffix, _}} ->
                        [File];
                    {error,_} ->
                        [File]
                end
        end,
    _ErrorFiles = filelib:fold_files(Path, "", true, F, []),
    ok.

delete_all(Host, Port) ->
    delete_all(Host, Port, ?SSMIX_BUCKET),
    delete_all(Host, Port, ?SSMIX_PATIENTS_BUCKET).

delete_all(Host, Port, Bucket) ->
    Self = self(),
    DeleterPid = spawn_link(fun() -> Self ! {self(), deleter(Host, Port, Bucket)} end),
    _FetcherPid = spawn_link(fun() -> Self ! {self(), fetcher(Host, Port, DeleterPid, Bucket)} end),
    
    receive
        {_, done} ->
            receive
                {_, done} -> ok
            end
    end.

-spec deleter(inet:hostname(), inet:port_number(), binary()) -> no_return().
deleter(Host, Port, Bucket0) ->
    {ok, C} = riakc_pb_socket:start_link(Host, Port),
    Bucket = case Bucket0 of
                 _ when is_binary(Bucket0) ->
                     {?BUCKET_TYPE, Bucket0};
                 _ when is_tuple(Bucket0) ->
                     Bucket0
             end,
    Result = deleter_loop(C, Bucket, 0),
    ok = riakc_pb_socket:stop(C),
    io:format("~p deleter: ~p~n", [Bucket, Result]),
    done.

-spec deleter_loop(pid(), binary(), non_neg_integer()) -> {ok, non_neg_integer()}.
deleter_loop(C, Bucket, Count) ->
    receive
        done -> {ok, Count};
        {fail, E} -> E;
        Keys when is_list(Keys) ->
            Fold = fun(Key, N) ->
                           {ok, RiakObj} = riakc_pb_socket:get(C, Bucket, Key),
                           ok = riakc_pb_socket:delete_obj(C, RiakObj, [{w,0}]),
                           N+1
                   end,
            Deleted = lists:foldl(Fold, 0, Keys),
            deleter_loop(C, Bucket, Deleted + Count)
    end.

-spec fetcher(atom(), inet:port_number(), pid(), binary()) -> no_return().
fetcher(Host, Port, DeleterPid, Bucket0) ->
    {ok, C} = riakc_pb_socket:start_link(Host, Port),
    Bucket = case Bucket0 of
                 _ when is_binary(Bucket0) ->
                     {?BUCKET_TYPE, Bucket0};
                 _ when is_tuple(Bucket0) ->
                     Bucket0
             end,
    case riakc_pb_socket:stream_list_keys(C, Bucket) of
        {ok, ReqID} ->
            Result = fetcher_loop(C, ReqID, 0, DeleterPid),
            ok = riakc_pb_socket:stop(C),
            io:format("~p fetcher: ~p > ~p~n", [Bucket, ReqID, Result]),
            DeleterPid ! done,
            done;
        {error, _} = E->
            DeleterPid ! {fail, E},
            fail
    end.

-spec fetcher_loop(pid(), riakc_pb_socket:req_id(),
                   non_neg_integer(), pid()) -> {ok, non_neg_integer()}.
fetcher_loop(C, ReqID, Count, DeleterPid) ->
    receive
        {ReqID, {keys, Keys}} ->
            DeleterPid ! Keys,
            fetcher_loop(C, ReqID, Count + length(Keys), DeleterPid);
        {ReqID, done} -> {ok, Count};
        {error, E} ->  io:format("~p", [E])
    end.

-spec process_file(filename:filename(), binary(), #context{}) ->  ok | {error, term()}.
process_file(File, HospitalID, #context{riakc=Riakc, logger=Logger} = _Ctx) ->
    treehugger:log(Logger, info, "Processing ~p ~p", [File, HospitalID]),
    case string:right(File, 2) of
        "_1" ->
            case hl7:from_file(File, Logger, dummy) of
                {ok, HL7Msg0} ->
                    HL7Msg = hl7:annotate(HL7Msg0#hl7msg{hospital_id=HospitalID}),
                    try
                        ok=md_record:put_json(Riakc, HL7Msg, hl7, Logger)
                    catch T:E ->
                            treehugger:log(Logger, error, "~p:~p ~p",
                                           [T,E, erlang:get_stacktrace()])
                    after
                        treehugger:log(Logger, info, "Processed ~s", [File])
                    end;
                {error, _Reason} = R ->
                    _ = treehugger:log(Logger, error, "~p:~p", [File, R]),
                    R
            end;
        _End ->
            treehugger:log(Logger, warning, "file ~s ignored because its suffix is not '_1'",
                           [File]),
            {error, {bad_suffix, File}}
    end.

-ifdef(TEST).
-endif.
