%% -*- coding: utf-8 -*-
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

%%% @doc
%%%  japanese multi-byte char handling
%%% @end

-module(japanese).

-include_lib("eunit/include/eunit.hrl").

%% -export([connect/2, disconnect/1, put/2]).
-export([read_file/1, fold_all_lines/3]).

%% @doc read a Shift_JIS/EUC-JP files and convert them with nkf
-spec read_file(filename:filename()) -> {ok, [string()]}.
read_file(File) ->
    {ok, Lines} = fold_all_lines(File, fun(Line, Acc) -> [Line|Acc] end, []),
    {ok, lists:reverse(Lines)}.
    %% Command = "nkf -w " ++ File,
    %% Port = open_port({spawn, Command}, [stream,in,binary,eof]),
    %% {ok, Lines} = get_all_lines(Port, <<>>),
    %% port_close(Port),
    %% {ok, Lines}.

%% get_all_lines(Port, Binary) ->
%%     receive
%%         {Port, {data, Data}} ->
%%             get_all_lines(Port, <<Binary/binary, Data/binary>>);
%%         {Port, eof} ->
%%             FileContent = unicode:characters_to_list(Binary),
%%             Lines = string:tokens(FileContent, "\r"),
%%             {ok, Lines}
%%     end.

-spec fold_all_lines(filename:filename(), fun(), term()) -> {ok, term()}.
fold_all_lines(File, Fun, Ctx0) ->
    Command = "nkf -w " ++ File,
    Port = open_port({spawn, Command}, [stream,in,binary,eof]),
    try
        fold_all_lines(Port, <<>>, Fun, Ctx0)
    after
        port_close(Port)
    end.

fold_all_lines(Port, Bin, Fun, Ctx0) ->
    receive
        {Port, {data, Data0}} ->
            Data = <<Bin/binary, Data0/binary>>,
            Lines = binary:split(Data, [<<"\r\n">>], [global]),
            {Last, Ctx} = handle_lines(Lines, Fun, Ctx0),
            fold_all_lines(Port, Last, Fun, Ctx);
        {Port, eof} ->
            case Bin of
                <<>> ->
                    {ok, Ctx0};
                _ ->
                    Line = unicode:characters_to_list(Bin),
                    {ok, Fun(Line, Ctx0)}
            end
    end.

handle_lines([], _, Acc) -> {<<>>, Acc};
handle_lines([Last], _, Acc) -> {Last, Acc};
handle_lines([H|L], Fun, Acc0) ->
    Line = unicode:characters_to_list(H),
    handle_lines(L, Fun, Fun(Line, Acc0)).

hankaku(UnicodeString) ->
    [ convert(UnicodeChar) || UnicodeChar <- UnicodeString ].


convert(UnicodeInt) when 65296 =< UnicodeInt andalso UnicodeInt =< 65306 ->
    UnicodeInt - 65296 + $0;
convert(UnicodeCharCapital) when 65313 =< UnicodeCharCapital
                                 andalso UnicodeCharCapital =< 65338 ->
    UnicodeCharCapital - 65313 + $A;
convert(UnicodeChar) when 65345 =< UnicodeChar
                          andalso UnicodeChar =< 65370 ->
    UnicodeChar - 65345 + $a;
convert(Char) when 0 =< Char andalso Char =< 255 ->
    Char.

-ifdef(TEST).

unicode_test() ->
    ?assertEqual("0123456789azAZ", hankaku("０１２３４５６７８９ａｚＡＺ")),
    ?assertEqual("0123456789azAZ", hankaku("0123456789azAZ")).

-endif.
