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

-module(klib).

-export([rev_map/2, maybe_binary/1,
         maybe_a2b/1, ensure_dir/1]).

-include_lib("kernel/include/file.hrl").

-spec rev_map(fun(), list()) -> list().
rev_map(F, L) ->
    Folder = fun(E, Acc) -> [F(E)|Acc] end,
    lists:foldl(Folder, [], L).

maybe_binary(Atom) when is_atom(Atom) ->
    atom_to_binary(Atom, utf8);
maybe_binary(Binary) ->
    Binary.

maybe_a2b(true) -> true;
maybe_a2b(false) -> false;
maybe_a2b(null) -> null;
maybe_a2b(Atom) -> maybe_binary(Atom).

ensure_dir(Dir) ->
    case file:read_file_info(Dir) of
        {ok, FileInfo} ->
            case FileInfo#file_info.type of
                directory -> ok;
                _ -> {error, {not_dir, Dir}}
            end;
        E -> {error, {E, Dir}}
    end.
