%%% @copyright (C) 2013, Basho Technologies Inc.,
%%% @doc
%%%
%%% @end
%%% Created : 17 Jul 2013 by UENISHI Kota <kota@basho.com>

-module(ssmix).

-compile(export_all).
-include_lib("kernel/include/file.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("ssmix.hrl").
-include("hl7.hrl").

walk(Path) ->
    walk(lists:reverse(filename:split(Path)), none, fun process_file/2).

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
            io:format("can't open file (~p):~s~n", [E, Path]);
        Other ->
            io:format("mmmmmm: ~p~n", [Other])
    end.

eval_dirfun(none, _, _) -> ok;
eval_dirfun(DirFun, Path, FileInfo) ->            
    catch DirFun(Path, FileInfo).

eval_filefun(none, _, _) -> ok;
eval_filefun(FileFun, Path, FileInfo) ->
    case  FileFun(Path, FileInfo) of
        ok -> ok;
        {error, R} ->
            io:format("~p: ~s~n", [R, Path]);
        Other ->
            erlang:display(Other),
            io:format("unknown_failure: ~s~n", [Path]),
            error
    end.
             

patient(Path, ID, _FirstSix) ->
    io:format("~s -> ~s~n", [Path, ID]),
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
    _Features = filename2msg(Filename),
    {ok, HL7Msg0} = hl7:parse(Filename, Info),
    %% TODO: compare Features and HL7Msg here

    %% TODO: print this as a JSON (or msgpack?)
    %% ?debugVal(HL7Msg),
    HL7Msg = HL7Msg0#hl7msg{file = list_to_binary(Filename)},
    JSON = hl7:to_json(HL7Msg),
    %%io:format("~s~n", [binary_to_list(JSON)]),
    ok = file:write_file(filename:basename(Filename) ++ ".json", JSON),
    %% io:format("<<< ~s >>>~n", [filename:basename(Filename)++".json"]),
    ok.

filename2msg(Fullpath) ->
    Elems = lists:reverse(filename:split(Fullpath)),
    [_, DT, Date, PID, _, _, HID|_] = Elems,
    #ssmix_msg{hid=HID, pid=PID, date=Date, datatype=DT, path=Fullpath}.
