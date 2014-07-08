%%%-------------------------------------------------------------------
%%% @author UENISHI Kota <kota@basho.com>
%%% @copyright (C) 2014, Basho
%%% @doc
%%%
%%% @end
%%% Created :  9 Feb 2014 by UENISHI Kota <kota@basho.com>
%%%-------------------------------------------------------------------
-module(klib).

-export([rev_map/2]).

-spec rev_map(fun(), list()) -> list().
rev_map(F, L) ->
    Folder = fun(E, Acc) -> [F(E)|Acc] end,
    lists:foldl(Folder, [], L).
