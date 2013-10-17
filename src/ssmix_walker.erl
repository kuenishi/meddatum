%%%-------------------------------------------------------------------
%%% @author UENISHI Kota <kota@basho.com>
%%% @copyright (C) 2013, UENISHI Kota
%%% @doc
%%%
%%% @end
%%% Created : 18 Oct 2013 by UENISHI Kota <kota@basho.com>
%%%-------------------------------------------------------------------
-module(ssmix_walker).

-behaviour(gen_server).

%% API
-export([start_link/1, pop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(TIMEOUT, 1).
-define(SERVER, ?MODULE). 
-include_lib("kernel/include/file.hrl").
-include("hl7.hrl").

-record(state, {
          paths=[] :: [filename:filename()],
          hl7_objects=[] :: [#hl7msg{}],
          kids =0  :: non_neg_integer()
         }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Path) ->
    gen_server:start_link(?MODULE, [Path], []).

pop(Pid) ->
    gen_server:call(Pid, pop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Path]) ->
    {ok, #state{paths=[Path]}, ?TIMEOUT}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(pop, _From, #state{paths=Paths, hl7_objects=Objs0, kids=N} = State0) ->
    State = State0#state{hl7_objects=[]},
    case Paths of
        [] when N =< 0 ->
            {stop, normal, {ok, Objs0}, State};
        [] ->
            {reply, {cont, Objs0}, State};
        _ ->
            {reply, {cont, Objs0}, State, ?TIMEOUT}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(timeout, #state{paths=[]} = State) ->
    {noreply, State};
handle_info(timeout, #state{paths=[Path|Paths], hl7_objects=Objs0, kids=N} = State) ->
    case file:read_file_info(Path) of
        {ok, #file_info{type=directory} = _FileInfo} ->
            case file:list_dir(Path) of
                {ok, Children} ->
                    Fullpaths = [ filename:join([Path, Child]) || Child <- Children ],
                    {noreply, State#state{paths=Fullpaths++Paths}, ?TIMEOUT};
                {error, _} = _R ->
                    %% TODO: output some log here
                    {noreply, State#state{paths=Paths}, ?TIMEOUT}
            end;
        {ok, #file_info{type=regular} = FileInfo} ->
            Self = self(),
            _Pid = spawn(fun() ->
                                 case hl7:parse(Path, FileInfo) of
                                     {ok, HL7Msg0} ->
                                         HL7Msg = hl7:annotate(HL7Msg0#hl7msg{file=list_to_binary(Path)}),
                                         Self ! {done, HL7Msg};
                                     {error, _} = R ->
                                         Self ! R
                                 end
                         end),
            {noreply, State#state{paths=Paths, hl7_objects=Objs0, kids=N+1}, ?TIMEOUT};

        {error, _Reason} = E ->
            io:format("can't open file (~p):~p~n", [E, Path]),
            {noreply, State#state{paths=Paths}, ?TIMEOUT};

        Other ->
            io:format("mmmmmm: ~p~n", [Other]),
            {noreply, State#state{paths=Paths}, ?TIMEOUT}
    end;
handle_info({done, HL7Msg}, #state{hl7_objects=Objs0, kids=N} = State) ->
    Objs = [HL7Msg | Objs0],
    {noreply, State#state{hl7_objects=Objs,kids=N-1}, ?TIMEOUT};
handle_info({error, _R}, State = #state{kids=N}) ->
    %% TODO: output log here
    {noreply, State#state{kids=N-1}, ?TIMEOUT}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
