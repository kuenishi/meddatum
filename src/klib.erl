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

-export([join/4,
         rev_map/2, maybe_binary/1,
         maybe_a2b/1, ensure_dir/1,
         str_to_numeric/1,
         epoch/0]).

-include_lib("kernel/include/file.hrl").

-spec join([tuple()], pos_integer(),
           [tuple()], pos_integer()) -> [{[tuple()], [tuple()]}].
join(Left, LN, Right, RN) ->
    join(Left, LN, Right, RN, []).

join([], _, [], _, Result) ->
    Result;
join(Left, LN, Right, RN, Result0) ->
    JoinKey = case Left of
                  [] -> element(RN, hd(Right));
                  _ -> element(LN, hd(Left))
              end,
    {RightTuples, RightRest} =
        keytake_all(JoinKey, RN, Right, []),
    {LeftTuples, LeftRest} =
        keytake_all(JoinKey, LN, Left, []),
    %% io:format("LeftTuples: ~p~n", [LeftTuples]),
    %% io:format("RightTuples: ~p~n", [RightTuples]),
    join(LeftRest, LN, RightRest, RN,
         [{LeftTuples, RightTuples}|Result0]).

keytake_all(Key, N, TupleList, Acc) ->
    case lists:keytake(Key, N, TupleList) of
        false -> {Acc, TupleList};
        {value, Tuple, TupleList1} ->
            keytake_all(Key, N, TupleList1, [Tuple|Acc])
    end.

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

-spec str_to_numeric(string()) -> integer() | float().
str_to_numeric(Str) -> 
    %% if decimal point omits from number, number is regarded as a integer.
    case string:chr(Str , $.) of
        0 -> list_to_integer(Str);
        _ -> str_to_nm_float(Str)
    end.

-spec str_to_nm_float(string()) -> float().
%% strip a sign
str_to_nm_float([$-|Str]) -> - str_to_nm_float(Str);
str_to_nm_float([$+|Str]) ->   str_to_nm_float(Str);
%% start with "."
str_to_nm_float([$.|_] = Str) -> str_to_nm_float([$0|Str]);
str_to_nm_float(Str) ->
    case lists:last(Str) =:= $. of
      true  -> list_to_float(lists:append(Str, "0")); %% end with "."
      false -> list_to_float(Str)
    end.


-spec epoch() -> non_neg_integer().
epoch() ->
    {MegaSecs, Secs, _} = os:timestamp(),
    MegaSecs * 1000000 + Secs.
