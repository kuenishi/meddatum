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

%% @doc Very small and simple logger to control output place frequently

-module(treehugger).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-export([log/3, log/4, stop/1]).

-record(state, {
          sink = lager :: lager | file:io_device(),
          zone = "" :: string()
         }).

-spec log(pid(), string(), list(term())) -> ok.
log(Pid, Msg, Term) ->
    log(Pid, info, Msg, Term).

-spec log(pid(), Level::atom(), string(), list(term())) -> ok.
log(Pid, Level, Msg, Term) ->
    gen_server:call(Pid, {log, Level, Msg, Term}).

-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:call(Pid, stop).

%% @doc
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init(Args) ->
    case proplists:get_value(output, Args) of
        undefined ->
            {ok, #state{sink=lager}};
        stdout ->
            {ok, #state{sink=standard_io}};
        stderr ->
            {ok, #state{sink=standard_error}};
        Atom when is_atom(Atom) ->
            {ok, #state{sink=Atom}};
        File ->
            case file:open(File, [append]) of
                {ok, IoDevice} ->
                    {ok, #state{sink=IoDevice,
                                zone=zone()}};
                Error ->
                    {stop, Error}
            end
    end.

%% @private
handle_call({log, Level, Msg, Term}, _From, #state{sink=lager} = State) ->
    _ = lager:log(Level, Msg, Term),
    {reply, ok, State};
handle_call({log, Level, Msg, Term}, _, #state{sink=IoDevice,
                                               zone=Zone} = State) ->
    Formatted = io_lib:format(Msg, Term),
    Reply = io:format(IoDevice, "~s [~s]: ~s~n",
                      [fmtnow(Zone), Level, Formatted]),
    {reply, Reply, State};
handle_call(stop, _, State) ->
    {stop, normal, ok, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc Format the current time into a string; copy from webmachine
-spec fmtnow(string()) -> string().
fmtnow(Zone) ->
    {{Year, Month, Date}, {Hour, Min, Sec}} = calendar:local_time(),
    io_lib:format("[~2..0w/~s/~4..0w:~2..0w:~2..0w:~2..0w ~s]",
                  [Date,month(Month),Year, Hour, Min, Sec, Zone]).

%% @doc Convert numeric month value to the abbreviation
-spec month(1..12) -> string().
month(1) ->
    "Jan";
month(2) ->
    "Feb";
month(3) ->
    "Mar";
month(4) ->
    "Apr";
month(5) ->
    "May";
month(6) ->
    "Jun";
month(7) ->
    "Jul";
month(8) ->
    "Aug";
month(9) ->
    "Sep";
month(10) ->
    "Oct";
month(11) ->
    "Nov";
month(12) ->
    "Dec".

-spec zone() -> string().
zone() ->
    Time = erlang:universaltime(),
    LocalTime = calendar:universal_time_to_local_time(Time),
    DiffSecs = calendar:datetime_to_gregorian_seconds(LocalTime) -
        calendar:datetime_to_gregorian_seconds(Time),
    zone((DiffSecs/3600)*100).

%% Ugly reformatting code to get times like +0000 and -1300

-spec zone(integer()) -> string().
zone(Val) when Val < 0 ->
    io_lib:format("-~4..0w", [trunc(abs(Val))]);
zone(Val) when Val >= 0 ->
    io_lib:format("+~4..0w", [trunc(abs(Val))]).
