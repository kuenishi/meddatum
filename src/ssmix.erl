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

-module(ssmix).

-compile(export_all).
-include_lib("kernel/include/file.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("ssmix.hrl").
-include("hl7.hrl").

walk(Path) ->
    walk2(Path).
%%    walk(lists:reverse(filename:split(Path)), none, fun process_file/2).

walk2(Path) ->
    {ok, Pid} = ssmix_walker:start_link(Path),
    {ok,C}=ssmix_importer:connect(localhost, 8087),
    timer:sleep(100),
    wait_for_walker(Pid, C, 0).

wait_for_walker(Pid, C, N) ->
    {Flag, HL7Msgs} = ssmix_walker:pop(Pid),
    %% ?debugVal(Flag),
    lists:foreach(fun(HL7Msg) ->
                          ok=ssmix_importer:put_json(C, HL7Msg)
                  end, HL7Msgs),
    N2 = length(HL7Msgs) + N,
    meddatum:log(info, "~p msgs stored.~n", [length(HL7Msgs)]),
    case Flag of
        ok -> ok=ssmix_importer:disconnect(C);
        cont -> wait_for_walker(Pid, C, N2)
    end.

-spec walk([string()], fun(), fun()) -> no_return().
walk(Path0, DirFun, FileFun)->
    Path = filename:join(lists:reverse(Path0)),
    %% erlang:display(Path),
    case file:read_file_info(Path) of
        {ok, #file_info{type=directory} = FileInfo} ->
            eval_dirfun(DirFun, Path, FileInfo),
            case file:list_dir(Path) of
                {ok, Children} ->
                    lists:foreach(fun(Child)->
                                          walk([Child|Path0], DirFun, FileFun)
                                  end, Children);
                _ ->
                    error
            end;
        {ok, #file_info{type=regular} = FileInfo} ->
            eval_filefun(FileFun, Path, FileInfo);
        {error, _Reason} = E ->
            meddatum:log(error, "can't open file (~p):~s~n", [E, Path]);
        Other ->
            meddatum:log(error, "mmmmmm: ~p~n", [Other])
    end.

eval_dirfun(none, _, _) -> ok;
eval_dirfun(DirFun, Path, FileInfo) ->            
    catch DirFun(Path, FileInfo).

eval_filefun(none, _, _) -> ok;
eval_filefun(FileFun, Path, FileInfo) ->
    case  FileFun(Path, FileInfo) of
        ok -> ok;
        {error, R} ->
            meddatum:log(error, "~p: ~s~n", [R, Path]);
        Other ->
            meddatum:log(fatal, "unknown_failure: ~s ~p~n", [Path, Other]),
            error
    end.
             

patient(Path, ID, _FirstSix) ->
    case file:list_dir(Path) of
        {ok, Formats} ->
            F = fun(Format)->
                        File = filename:join([Path, Format]),
                        hl7:parse(File, ID, Format)
                end,
            lists:foreach(F, Formats);
        _ ->
            none
    end.



-spec process_file(filename:filename(), file:file_info()) -> ok | {error, any()}.
process_file(Filename, Info)->
    {ok, HL7Msg0} = hl7:parse(Filename, Info),

    %% TODO: print this as a JSON (or msgpack?)
    HL7Msg = hl7:annotate(HL7Msg0#hl7msg{file=list_to_binary(Filename)}),
    %% JSON = hl7:to_json(HL7Msg),
    %% ok = file:write_file(filename:basename(Filename) ++ ".json", JSON),
    meddatum:log(info, "output ~s to Riak as JSON~n",
                 [filename:basename(HL7Msg#hl7msg.file)]),
    %io:put_chars(unicode:characters_to_list(JSON)),
    {ok,C}=ssmix_importer:connect(localhost, 8087),
    ok=ssmix_importer:put_json(C, HL7Msg),
    ok=ssmix_importer:disconnect(C).

filename2msg(Fullpath) ->
    Elems = lists:reverse(filename:split(Fullpath)),
    [_, DT, Date, PID, _, _, HID|_] = Elems,
    #ssmix_msg{hospital_id=HID, patient_id=PID, date=Date,
               datatype=DT, path=Fullpath}.
