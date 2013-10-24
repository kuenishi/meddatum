%%%-------------------------------------------------------------------
%%% @author UENISHI Kota <kota@basho.com>
%%% @copyright (C) 2013, UENISHI Kota
%%% @doc
%%%
%%% @end
%%% Created :  8 Sep 2013 by UENISHI Kota <kota@basho.com>
%%%-------------------------------------------------------------------
-module(md_searcher).

-export([plain_query_url/2, run_query/3, run_query0/2,
         simple_doc_retriever/1,
         get_property/2,
         simple_offset_counter/2, simple_doc_mapper/1]).

-include_lib("eunit/include/eunit.hrl").

plain_query_url(Server, Query0) ->
    UrlBase = io_lib:format("http://~s:8098/", [Server]),
    Query = (re:replace(Query0, "[\s]", "%20", [unicode,{return,list},global])),
    Params = "wt=json&"++Query,
    lists:flatten(UrlBase ++ "search/md_index?"++Params).

-spec run_query(Url::string(), non_neg_integer(),
                fun()) ->
                   {ok, {non_neg_integer(), [{binary(),binary()}]}}.
run_query(Url0, Offset, DocRetriever) ->
    Json = run_query0(Url0, Offset),
    {ok, {_Num, _BKs}} = DocRetriever(Json).


run_query0(Url0, Offset) ->
    Url = Url0 ++ "&start=" ++ integer_to_list(Offset),
    %% ?debugVal(Url),
    Res = ibrowse:send_req(Url, [], get),
    {ok, "200", _Header, Body} = Res,
    
    _Json = jsonx:decode(unicode:characters_to_binary(Body), []).
    

get_property(Key, JsonxObject) ->
    {Proplist} = JsonxObject,
    proplists:get_value(Key, Proplist).

simple_doc_retriever(Json) ->
    Response = get_property(<<"response">>, Json),
    Docs = get_property(<<"docs">>, Response),

    _MaxScore = get_property(<<"maxScore">>, Response),
    NumFound = get_property(<<"numFound">>, Response),

    BKs = lists:map(fun simple_doc_mapper/1, Docs),
    Offset = simple_offset_counter(NumFound, Docs),
    {ok, {Offset, BKs}}.

simple_offset_counter(Num, _Docs) -> Num.

simple_doc_mapper(Doc) ->
    B = get_property(<<"_yz_rb">>, Doc),
    K = get_property(<<"_yz_rk">>, Doc),
    {B, K}.
        
