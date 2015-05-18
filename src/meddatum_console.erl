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

-module(meddatum_console).

-include("meddatum.hrl").
-include_lib("riakc/include/riakc.hrl").

-export([setup/0, setup/1, teardown/1]).

-export([create_config/0, check_config/0,
         setup_riak/0,
         import_ssmix/1, import_recept/1,
         import_dpcs/2,
         parse_ssmix/1, parse_recept/1,
         parse_dpcs/1,
         delete_all_ssmix/1, delete_recept/1,
         search/1]).

setup() ->
    setup(true).

setup(NeedRiak) when is_boolean(NeedRiak) ->

    %% ok = error_logger:tty(false),
    {ok, Pid} = treehugger:start_link([{output, standard_io}]),
    treehugger:log(Pid, "starting meddatum", []),
    {ok, Config} = meddatum_config:get_config(),

    {ok, {Host, Port}} = meddatum_config:get_riak(Config),
    {ok, Riakc} =
        case NeedRiak of
            true -> riakc_pb_socket:start_link(Host, Port);
            false -> {ok, undefined}
        end,
    {ok, #context{logger=Pid, riakc=Riakc, config=Config}}.

teardown(#context{logger=Pid, riakc=Riakc} = _Ctx) ->
    ok = riakc_pb_socket:stop(Riakc),
    ok = treehugger:stop(Pid),
    ok.

create_config() ->
    Filename = os:getenv("HOME")++"/.meddatum",
    case file:open(Filename, [write, exclusive]) of
        {ok, IoDevice} ->
            String =
                "{riak_ip, \"127.0.0.1\"}.\n"
                "{riak_port, 8087}.\n",
            ok = file:write(IoDevice, String),
            ok = file:close(IoDevice),
            io:format("Created ~s~n", [Filename]);
        {error, _} = E ->
            io:format("Can't create file ~s: ~p~n",
                      [Filename, E])
    end.

check_config() ->
    Filename = os:getenv("HOME")++"/.meddatum",
    case file:consult(Filename) of
        {error, Reason} ->
            io:format("~p: ~s~n", [Filename, file:format_error(Reason)]);
        {ok, _} ->
            true = meddatum_config:check_riak_connectivity(),
            true = meddatum:check_setup(),
            io:format("ok~n")
    end.

setup_riak() ->
    {ok, {Host,Port}} = meddatum_config:get_riak(),
    meddatum:setup(Host, Port).

import_ssmix([HospitalID, Path]) ->
    {ok, #context{logger=Logger} = Ctx} = setup(),
    try
        ssmix_importer:walk(Path, HospitalID, Ctx)
    catch E:T ->
            _ = treehugger:log(Logger, error, "~p:~p @ ~s > ~p", [E, T, Path, erlang:get_stacktrace()])
    after
        _ = treehugger:log(Logger, info, "finished processing: ~p", [Path]),
        teardown(Ctx)
    end;

import_ssmix(_) -> meddatum:help().

import_recept([Mode0, Filename]) ->
    Mode = case Mode0 of
               "dpc" -> dpc;
               "med" -> med
           end,
    {ok, #context{logger=Logger,
                  riakc=C} = Context} = meddatum_console:setup(),
    treehugger:log(Logger, info, "parsing ~s as ~s", [Filename, Mode]),
    try
        {ok, Records} = rezept:from_file(Filename, [Mode], Logger),
        treehugger:log(Logger, info,
                       "parsing ~p finished (~p records extracted)",
                       [Filename, length(Records)]),

        lists:foreach(fun(Record)->
                              ok = md_record:put_json(C, Record, rezept, Logger)
                      end, Records),

        treehugger:log(Logger, info, "wrote ~p records into Riak.", [length(Records)])
    catch E:T ->
            treehugger:log(Logger, error,
                           "~p:~p ~w", [E, T, erlang:get_stacktrace()])
    after
        meddatum_console:teardown(Context)
    end;
import_recept(_) -> meddatum:help().

import_dpcs([_Dir, HospitalID, Date|_] = Argv, Force) ->

    Identifier = {HospitalID, Date},
    case dpcs:files_to_parse(Argv) of
        {error, _} = E -> E;
        {ok, Files} ->
            {ok, #context{logger=Logger,
                          riakc=C} = Context} = meddatum_console:setup(),
            treehugger:log(Logger, info, "parsing ~s", [Files]),
            case Force of
                true -> ok;
                false -> md_record:check_is_set_done(C, dpcs, Identifier)
            end,
            try
                case dpcs:parse_files(Files, HospitalID, Date, Logger) of
                    {ok, Records} ->
                        treehugger:log(Logger, info,
                                       "parsing ~p finished", [Files]),
                        lists:foreach(fun(Record)->
                                              ok = md_record:put_json(C, Record, dpcs, Logger)
                                      end, Records),
                        case md_record:mark_set_as_done(C, dpcs, Identifier) of
                            true ->
                                treehugger:log(Logger, info, "wrote ~p records into Riak.", [length(Records)]);
                            _ ->
                                treehugger:log(Logger, info, "failed writing records into Riak.", [length(Records)])
                        end
                end
            catch E:T ->
                    treehugger:log(Logger, error,
                                   "~p:~p ~w", [E, T, erlang:get_stacktrace()])
            after
                meddatum_console:teardown(Context)
            end
    end;

import_dpcs(_, _) -> meddatum:help().

parse_ssmix([Path]) ->
    io:setopts([{encoding,utf8}]),

    {ok, #context{logger=Logger}} = meddatum_console:setup(false),

    F = fun(File, Acc0) ->
                case hl7:from_file(File, Logger, dummy) of
                    {ok, HL7Msg0} ->
                        io:format(standard_error, "~ts:~n", [File]),
                        io:format("~ts~n", [element(2, hl7:to_json(HL7Msg0))]);
                    {error,_} when is_list(Acc0) ->
                        [File|Acc0];
                    {error,_} ->
                        [File]
                end
        end,
    case filelib:is_dir(Path) of
        true ->
            _ErrorFiles = filelib:fold_files(Path, "", true, F, []);
        false ->
            case filelib:is_file(Path) of
                true ->
                    F(Path, []);
                false ->
                    treehugger:log(Logger, error, "No file found:~p", [Path])
            end
    end;

parse_ssmix(_) -> meddatum:help().

parse_recept([Mode, File]) ->
    io:setopts([{encoding, unicode}]),
    io:format(standard_error, "~p~n", [File]),
    ModeAtom = case Mode of
                   "dpc" -> dpc;
                   "med" -> med
               end,

    {ok, #context{logger=Logger}} = meddatum_console:setup(false),

    {ok, Records} = rezept:from_file(File, [ModeAtom], Logger),
    lists:foreach(fun({ok, JSON}) ->
                          io:format("~ts~n", [JSON]);
                     ({error, E}) ->
                          io:format("error: ~p~n", [E])
                  end,
                  lists:map(fun rezept:to_json/1, lists:reverse(Records)));

parse_recept(_) -> meddatum:help().

parse_dpcs([_Dir, HospitalID, Date|_] = Argv) ->
    io:setopts([{encoding, unicode}]),
    {ok, #context{logger=Logger} = Context} = meddatum_console:setup(),

    {ok, Files} = dpcs:files_to_parse(Argv),
    io:format(standard_error, "Files to parse: ~p~n", [Files]),
    BinHospitalID = list_to_binary(HospitalID),
    YYYYMM = iolist_to_binary(["20", Date]),
    try
        {ok, ModesRecords} = dpcs:parse_files(Files, BinHospitalID, YYYYMM, Logger),
        lists:foreach(fun({_Mode, Records})->
                              lists:foreach(fun(Record) ->
                                                    io:format("~ts~n", [element(2, dpcs:to_json(Record))])
                                            end, Records)
                      end, ModesRecords)
    after
        meddatum_console:teardown(Context)
    end;

parse_dpcs(_) -> meddatum:help().


delete_all_ssmix(_) -> io:format("TBD~n").

delete_recept([File]) ->
    io:setopts([{encoding, unicode}]),
    io:format("deleting: ~p~n", [File]),
    {ok, {Host, Port}} = meddatum_config:get_riak(),
    %% If you use a riak&healthb younger than pre5 and 0.1.x
    %% turn this boolean to false, will be not using bucket types
    application:set_env(meddatum, use_bucket_types, true),
    {ok, C} = riakc_pb_socket:start_link(Host, Port),
    case rezept_io:delete_from_file(C, File) of
        {ok, {OK,E}} ->
            io:format("~p records deleted (~p failed).", [OK, E]);
        E ->
            io:format("can't retrieve any keys from file ~p (~p)", [File, E])
    end,
    ok = riakc_pb_socket:stop(C).

search([Query]) ->
    {ok, {Host, Port}} = meddatum_config:get_riak(),
    {ok, C} = riakc_pb_socket:start_link(Host, Port),
    case riakc_pb_socket:search(C, ?INDEX_NAME, Query) of
        {ok, #search_results{docs=Docs, num_found=Found} = _SearchResults} ->
            io:format("~p docs found:~n", [Found]),
            [ format_doc(Doc) || Doc <- Docs];
        E ->
            io:format("can't search with query ~s: ~p", [Query, E])
    end,
    ok = riakc_pb_socket:stop(C).

%% === internal ===

format_doc({?INDEX_NAME, Data}) ->
    T = proplists:get_value(<<"_yz_rt">>, Data),
    B = proplists:get_value(<<"_yz_rb">>, Data),
    K = proplists:get_value(<<"_yz_rk">>, Data),
    io:format("types/~s/buckets/~s/keys/~s: ", [T,B,K]),
    Result = chopper([<<"_yz_rt">>, <<"_yz_rb">>, <<"_yz_rk">>,
                      <<"_yz_id">>, <<"score">>], Data),
    io:format("~p~n", [Result]);
format_doc(U) ->
    io:format(standard_error, "unknown: ~p~n", [U]).

chopper([], Proplist) -> Proplist;
chopper([H|L], Proplist) ->
    chopper(L, proplists:delete(H, Proplist)).
