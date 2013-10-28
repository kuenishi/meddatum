%%% @copyright (C) 2013, Basho Technologies Inc.,
%%% @doc
%%%  japanese multi-byte char handling
%%% @end
%%% Created : 2 Aug 2013 by UENISHI Kota <kota@basho.com>

-module(japanese).

-include_lib("eunit/include/eunit.hrl").

%% -export([connect/2, disconnect/1, put/2]).
-export([read_file/1, hankaku/1]).

%% @doc read a Shift_JIS/EUC-JP files and convert them with nkf
-spec read_file(filename:filename()) -> {ok, [string()]}.
read_file(File) ->
    Command = "nkf -w " ++ File,
    Port = open_port({spawn, Command}, [stream,in,binary,eof]),
    {ok, Lines} = get_all_lines(Port, <<>>),
    port_close(Port),
    {ok, Lines}.

get_all_lines(Port, Binary) ->
    receive
        {Port, {data, Data}} ->
            get_all_lines(Port, <<Binary/binary, Data/binary>>);
        {Port, eof} ->
            FileContent = unicode:characters_to_list(Binary),
            Lines = string:tokens(FileContent, "\r"),
            {ok, Lines};
        Other -> ?debugVal(Other)
    end.

hankaku(UnicodeString) ->
    [ convert(UnicodeChar) || UnicodeChar <- UnicodeString ].


convert(UnicodeInt) when 65296 =< UnicodeInt andalso UnicodeInt =< 65306 ->
    UnicodeInt - 65296 + $0;
convert(UnicodeCharCapital) when 65313 =< UnicodeCharCapital
                                 andalso UnicodeCharCapital =< 65338 ->
    UnicodeCharCapital - 65313 + $A;
convert(UnicodeChar) when 65345 =< UnicodeChar
                          andalso UnicodeChar =< 65370 ->
    UnicodeChar - 65345 + $a.

-ifdef(TEST).

unicode_test() ->
    UnicodeString = unicode:characters_to_list(<<"０１２３４５６７８９ａｚＡＺ">>),
    ?assertEqual("0123456789azAZ", hankaku(UnicodeString)).

-endif.
