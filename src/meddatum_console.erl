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

-export([setup/0, setup/1, teardown/1]).

-export([create_config/0, check_config/0,
         setup_riak/0,
         import_ssmix/1, import_recept/1,
         parse_ssmix/1, parse_recept/1,
         delete_all_ssmix/1, delete_recept/1]).

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
                              ok = rezept_io:put_record(C, Record)
                      end, Records),

        treehugger:log(Logger, info, "wrote ~p records into Riak.", [length(Records)])
    catch E:T ->
            treehugger:log(Logger, error,
                           "~p:~p ~w", [E, T, erlang:get_stacktrace()])
    after
        meddatum_console:teardown(Context)
    end;
import_recept(_) -> meddatum:help().

parse_ssmix([Path]) ->
    io:setopts([{encoding,utf8}]),

    {ok, #context{logger=Logger}} = meddatum_console:setup(false),

    F = fun(File, Acc0) ->
                case hl7:from_file(File, Logger) of
                    {ok, HL7Msg0} ->
                        io:format(standard_error, "~ts:~n", [File]),
                        io:format("~ts~n", [hl7:to_json(HL7Msg0)]);
                    {error,_} when is_list(Acc0) ->
                        [File|Acc0];
                    {error,_} ->
                        [File]
                end
        end,
    _ErrorFiles = filelib:fold_files(Path, "", true, F, []);
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

%% === internal ===
