%%
%% Copyright (C) 2013-2014 UENISHI Kota
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

-module(hl7_parser).

-export([parse/2, parse/3]).

-include_lib("eunit/include/eunit.hrl").
-include("hl7.hrl").
-include("hl7_types.hrl").

-spec parse(filename:filename(), file:file_info()) ->
                   {ok, #hl7msg{}} | {error, any()}.
parse(Filename, _Info)->
    {ok, HL7Msg0} = parse_msg(Filename),
    Date = filename_to_date(Filename),
    {ok, HL7Msg0#hl7msg{file=list_to_binary(Filename),
                        date=Date}}.

parse(Filename, Info, undefined)->
    parse(Filename, Info);
parse(Filename, Info, PostProcessor) when is_function(PostProcessor, 1) ->
    {ok, HL7Msg0} = parse(Filename, Info),
    {ok, PostProcessor(HL7Msg0)}.

filename_to_date(Filename) when is_binary(Filename) ->
    filename_to_date(binary_to_list(Filename));
filename_to_date(Filename) when is_list(Filename) ->
    Tokens = string:tokens(Filename, "_"),
    case length(Tokens) of
        L when L >= 2 ->
            list_to_binary(lists:nth(2, Tokens));
        _ ->
            undefined
    end.

parse_msg(File)->
    case read_all_lines(File) of %% I NEED SIMPLE MAYBE MONAD
        {ok, []} ->
            {error, {empty, File}};
        {ok, Lines0} ->
            case parse_0(Lines0) of
                {ok, Msg} ->
                    parse_1(Msg, tl(Lines0), File);
                {error, _} = E ->
                    %% TODO: output log here
                    E
            end;
        {error, _} = E ->
            %% TODO: output log here
            E
    end.

read_all_lines(File) ->
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
        Other ->
            lager:error("~p", [Other])
    end.
             
parse_0([Line|_Lines]) ->
    %% case binary:split(Line, <<"|">>, [global]) of
    %% case string:tokens(Line, "|") of
    Tokens = re:split(Line, "[|]", [{return,list},unicode]),
    case Tokens of
        ["MSH"|_] = Tokens ->
            Msg = 'MSH'(Tokens),
            {ok, Msg};
        O ->
            {error, {bad_format, O}}
    end.


%%     [ 日本語どうでしたっけ？いける！！
parse_1(#hl7msg{segments=Segs} = Msg, [], _) ->
    {ok, Msg#hl7msg{segments=lists:reverse(Segs)}};

parse_1(Msg, [Line|Lines] = _Lines, File) ->
    Tokens = re:split(Line, "[|]", [{return,list},unicode]),
    case hd(Tokens) of
        Segment when is_list(Segment) ->
            {ok, NewMsg} = handle_segment_0(Segment, Tokens, Msg, File),
            parse_1(NewMsg, Lines, File);

        _Other ->
            lager:error("unknown segment: ~ts", [_Other]),
            {error, badarg}
    end.



%% charcode is defined as ISO IR87 / JIS Kanji code.

%% From page 24 of SS-MIX2 標準化ストレージ仕様書
'MSH'(Tokens) ->
    ["MSH", _Seperators, _SenderApp, _Sender, _ReceiverApp, _Receiver,
     _Date, _Security, _MsgType, _MsgID, _ProcessID,
     "2.5",  %% Version ID is fixed
     _SeqNum, _ContPointer, _YN, _, _CountryCode,
     "~ISO IR87", %% Charcode is fixed (but already translated to UTF8 :)
     _Lang,
     "ISO 2022-1994"|_] = Tokens,
    #hl7msg{msg_type_s = maybe_nth(9, Tokens),
            msg_id = maybe_nth(10, Tokens)}.


handle_segment_0(MsgType, Tokens0, Msg, File) ->
    case proplists:get_value(MsgType, ?HL7_TYPES) of
        undefined -> {ok, Msg};
        MsgDef0 ->
            ShortLen = erlang:min(length(MsgDef0), length(Tokens0)),
            {MsgDef, _} = lists:split(ShortLen, MsgDef0),
            {Tokens, _} = lists:split(ShortLen, Tokens0),

            Data0 = lists:map(fun({{Property, {maybe, _Type}, _Length, _Text}, ""}) ->
                                      %% ok to skip
                                      {Property, null};
                                 ({{Property, {maybe, Type}, Length, _Text}, Col}) ->
                                      to_json_object(Property, Type, Length, Col);

                                 ({{Property, _Type, _Length, _Text}, ""}) ->
                                      lager:warning("empty property '~s' which isn't optional in ~s~n",
                                                    [Property, File]),
                                      {Property, null};
                                      
                                 ({{Property, Type, Length, _Text}, Col}) ->
                                      %% ?debugVal({Property, Type}),
                                      to_json_object(Property, Type, Length, Col)
                      end,
                      lists:zip(MsgDef, Tokens)),
            Data = lists:filter(fun({_,null}) -> false;
                                   (_) -> true end, Data0),

            case get_observation_value(Data) of
                undefined ->
                    {ok, append_segment(Msg, {Data})};
                {'*', Col} -> %% special case for OBX-5
                    Data1 = case get_valuetype(Data) of
                                undefined ->
                                    delete_valuetype(Data);
                                ValueType ->
                                    Value = to_json_object(binary_to_atom(ValueType, latin1), Col, 1),
                                    [{<<"observation_value">>, Value}|delete_valuetype(Data)]
                            end,
                    {ok, append_segment(Msg, {Data1})}
            end
    end.

get_valuetype(Data) ->
    case proplists:get_value(<<"valuetype ">>, Data) of
        undefined -> proplists:get_value(<<"valuetype">>, Data);
        ValueType -> ValueType
    end.
get_observation_value(Data) ->
    case proplists:get_value(<<"observation_value ">>, Data) of
        undefined -> proplists:get_value(<<"observation_value">>, Data);
        Value -> Value
    end.

delete_valuetype(Data) ->
    proplists:delete(<<"observation_value ">>,
                     proplists:delete(<<"observation_value">>, Data)).
    

to_json_object(Property, Type, _Len, Col) ->
    %% DAMN NONSENSE GUARD
    %% if length(Col) > Len ->
    %%         error({"too long text", Col, Len});
    %%    true ->
    %% split repeat
    %% get_separator(1) -> "[~]";
    BinProperty = list_to_binary(Property),
    case re:split(Col, "[~]", [{return,list},unicode]) of
        [] ->
            {BinProperty, null};
        [Token] ->
            {BinProperty, to_json_object(Type, Token, 0)};
        Tokens when is_list(Tokens) ->
            {BinProperty, [to_json_object(Type, T, 0) || T <- Tokens]}
    end.

to_json_object('ST', Col, _D)-> unicode:characters_to_binary(Col);
to_json_object('TX', Col, _D)-> unicode:characters_to_binary(Col);
to_json_object('FT', Col, _D)-> unicode:characters_to_binary(Col);
to_json_object('NM', Col, _D)-> case catch list_to_integer(Col) of
                                    I when is_integer(I) -> I;
                                    _ -> list_to_float(Col)
                                end;
to_json_object('IS', Col, _D)-> unicode:characters_to_binary(Col); %%  list_to_binary(Col);
to_json_object('ID', Col, _D)-> unicode:characters_to_binary(Col); %% list_to_binary(Col);
to_json_object('DT', Col, _D)-> unicode:characters_to_binary(Col); %% 「小学校低学年の頃」
to_json_object('TM', Col, _D)-> list_to_binary(Col);
to_json_object('DTM', Col, _D)-> list_to_binary(Col);
to_json_object('SI', Col, _D)-> list_to_integer(Col);

%% Work arounds
to_json_object('*', Col, _) -> {'*', Col}; %% as it is and process later
to_json_object('FN', Col, _D)->  unicode:characters_to_binary(Col); %% undefined
to_json_object('SAD', Col, _D)-> unicode:characters_to_binary(Col); %% undefined
to_json_object('SPS', Col, _D)-> unicode:characters_to_binary(Col); %% undefined, maybe, and exists.
to_json_object('AUI', Col, _D)-> unicode:characters_to_binary(Col); %% undefined, maybe, and exists.

to_json_object(Name, Col, Depth)->
    to_record(Name, Col, Depth).

get_separator(0) -> "[\\^]";
get_separator(1) -> "[\\\\]";
get_separator(2) -> "[&]".

to_record(Name, Col, Depth) ->
    Tokens0 = re:split(Col, get_separator(Depth), [{return,list},unicode]),
    case proplists:get_value(Name, ?HL7_PRIMITIVE_TYPES) of
        undefined ->
            error({unknown, Name});

        TypeDef0 ->
            ShortLen = erlang:min(length(TypeDef0), length(Tokens0)),
            {TypeDef, _} = lists:split(ShortLen, TypeDef0),
            {Tokens, _}  = lists:split(ShortLen, Tokens0),

            Data0 = lists:map(fun({{Property, _Type}, []}) ->
                                      {klib:maybe_binary(Property), null};
                                 ({{Property, Type}, Tok}) ->
                                      {klib:maybe_binary(Property),
                                       to_json_object(Type, Tok, Depth+1)}
                              end,
                              lists:zip(TypeDef, Tokens)),
            Data = lists:filter(fun({_,null}) -> false;
                                   (_) -> true end, Data0),
            {Data} %% jsone requres {_} as JSON object
    end.

append_segment(#hl7msg{segments = Segs} = Msg, Seg) ->
    Msg#hl7msg{segments = [Seg|Segs]}.

maybe_nth(N, List) ->
    try
        case lists:nth(N, List) of
            "" -> null;
            Term -> unicode:characters_to_binary(Term)
        end
    catch _:_ ->
            null
    end.
