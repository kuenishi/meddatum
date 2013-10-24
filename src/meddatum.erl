%%% @author UENISHI Kota <kota@basho.com>
%%% @copyright (C) 2013, UENISHI Kota
%%% @doc
%%%
%%% @end
%%% Created :  8 Sep 2013 by UENISHI Kota <kota@basho.com>

-module(meddatum).

-export([search/2, search/3]).

-include_lib("eunit/include/eunit.hrl").

search(Server, Query) ->
    search(Server, Query,
           fun md_searcher:simple_doc_retriever/1).

search(Server, Query, DocRetriever) ->
    ?debugHere,
    Self = self(),
    Pid = spawn_link(fun() -> consumer(Server, waiting, Self) end),
    ibrowse:start(),
    io:setopts([{encoding, unicode}]),
    Url = md_searcher:plain_query_url(Server, Query),
    {ok, {NumFound, BKs}} = md_searcher:run_query(Url, 0, DocRetriever),
    io:format(standard_error, "sending query to ~p~n", [Url]),
    Pid ! {start, NumFound, BKs},

    io:format(standard_error, "~p results found (~p).~n", [NumFound, length(BKs)]),

    pagenate(Url, NumFound, length(BKs), Pid, DocRetriever),
    
    %% io:format("~p~n", [Docs]),
    ibrowse:stop(),
    %% Result = riakc_pb_socket:search(C, <<"md_index">>, list_to_binary(Query)),
    %% io:format("~p~n", [Result]),
    receive done ->
            io:format(standard_error, "all data yeilded.~n", [])
    end.


pagenate(Url0, _NumFound0, Offset, Pid, DocRetriever) ->
    {ok, {NumFound, BKs}} = md_searcher:run_query(Url0, Offset, DocRetriever),

    if length(BKs) > 0 ->
            Pid ! {data, BKs},
            pagenate(Url0, NumFound, Offset+length(BKs), Pid, DocRetriever);
       true ->
            ok
    end.

consumer(Server, waiting, Parent) ->
    {ok, C} = riakc_pb_socket:start_link(Server, 8087),
    receive
        {start, NumFound, Docs} ->
            io:format(standard_error, "start processing ~p/~p records found.~n",
                      [length(Docs), NumFound]),
            lists:foreach(fun(Doc) -> print_doc(C, Doc) end, Docs),
            consumer(C, {waiting, NumFound-length(Docs)}, Parent)
    end;
consumer(C, {waiting, Remain}, Parent) when Remain > 0 ->
    receive
        {data, Docs} ->
            io:format(standard_error, "processing ~p/~p records found.~n",
                      [length(Docs), Remain]),
            lists:foreach(fun(Doc) -> print_doc(C, Doc) end, Docs),
            consumer(C, {waiting, Remain-length(Docs)}, Parent)
    end;
consumer(C, {waiting, _}, Parent) ->
    %% done
    riakc_pb_socket:stop(C),
    Parent ! done.

print_doc(C, {B,K}) ->
    {ok, RiakObj} = riakc_pb_socket:get(C, B, K),
    V = riakc_obj:get_value(RiakObj),
    io:format(standard_error, "[info] ~p ~p (~p bytes)~n", [B, K, byte_size(V)]),
    case unicode:characters_to_list(V, utf8) of
        E when is_tuple(E) ->
            {error, _, T} = E,
            io:format(standard_error, "error ~p~n", [binary_to_term(T)]),
            exit(E);
        String when is_list(String) ->
            io:put_chars(String),
            io:format("~n");
        Other ->
            io:format(standard_error, "unknown: ~p~n", [Other])

    end.
    
