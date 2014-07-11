%%%-------------------------------------------------------------------
%%% @author UENISHI Kota <kota@basho.com>
%%% @copyright (C) 2014, Basho
%%% @doc
%%%
%%% @end
%%% Created :  9 Feb 2014 by UENISHI Kota <kota@basho.com>
%%%-------------------------------------------------------------------
-module(klib).

-export([rev_map/2, maybe_binary/1, ensure_dir/1]).

-include_lib("kernel/include/file.hrl").

-spec rev_map(fun(), list()) -> list().
rev_map(F, L) ->
    Folder = fun(E, Acc) -> [F(E)|Acc] end,
    lists:foldl(Folder, [], L).

maybe_binary(Atom) when is_atom(Atom) ->
    atom_to_binary(Atom, utf8);
maybe_binary(Binary) ->
    Binary.

ensure_dir(Dir) ->
    case file:read_file_info(Dir) of
        {ok, FileInfo} ->
            case FileInfo#file_info.type of
                directory -> ok;
                _ -> {error, {not_dir, Dir}}
            end;
        E -> {error, {E, Dir}}
    end.
