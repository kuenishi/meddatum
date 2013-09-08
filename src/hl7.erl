%%% @copyright (C) 2013, Basho Technologies Inc.,
%%% @doc
%%%  HL7 importer, transformer.
%%% @end
%%% Created : 17 Jul 2013 by UENISHI Kota <kota@basho.com>

-module(hl7).

-export([parse/2, to_json/1]).
-export_type(['ST'/0, 'TX'/0, 'FT'/0, 'NM'/0, 'IS'/0, 'ID'/0,
              'HD'/0, 'CE'/0, 'CNE'/0, 'CWE'/0, 'DT'/0, 'TM'/0,
              'DTM'/0,
              'TS'/0, 'DR'/0, 'MSG'/0, 'PT'/0, 'VID'/0, 'XCN'/0,
              'CX'/0, 'XPN'/0, 'XAD'/0, 'XTN'/0, 'XON'/0, 'PL'/0,
              'EI'/0, 'RPT'/0, 'CQ'/0, 'LA2'/0, 'EIP'/0, 'SI'/0,
              'JCC'/0, 'ZRD'/0]).

-include_lib("eunit/include/eunit.hrl").
-include("hl7.hrl").
-include("hl7_types.hrl").

-spec parse(filename:filename(), file:file_info()) -> ok | {error, any()}.
parse(Filename, _Info)->
    parse_msg(Filename).

parse_msg(File)->
    %%?debugVal(File),
    case read_all_lines(File) of %% I NEED SIMPLE MAYBE MONAD
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
        Other -> ?debugVal(Other)
    end.
             

parse_0([Line|_Lines]) ->
    %% lists:foreach(fun(L)->pp(L)end, Lines),
    %% case binary:split(Line, <<"|">>, [global]) of
    %% case string:tokens(Line, "|") of
    Tokens = re:split(Line, "[|]", [{return,list},unicode]),
    case Tokens of
        ["MSH"|_] = Tokens ->
            Msg = 'MSH'(Tokens),
            {ok, Msg};
        _O ->
            erlang:display(_O),
            {error, bad_format}
    end.


%%     [ 日本語どうでしたっけ？いける！！
parse_1(#hl7msg{segments=Segs} = Msg, [], _) ->
    {ok, Msg#hl7msg{segments=lists:reverse(Segs)}};

parse_1(Msg, [Line|Lines] = _Lines, File) ->
    Tokens = re:split(Line, "[|]", [{return,list},unicode]),
    case hd(Tokens) of
        Segment when is_list(Segment) ->
            %% io:put_chars(Line),
            {ok, NewMsg} = handle_segment_0(Segment, Tokens, Msg, File),
            parse_1(NewMsg, Lines, File);

        _Other ->
            io:format("unknown segment: ~s", [_Other]),
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
    %% ?debugVal(length(Tokens)),
    #hl7msg{date = maybe_nth(7, Tokens),
            msg_type_s = maybe_nth(9, Tokens),
            msg_id = maybe_nth(10, Tokens)}.


handle_segment_0(MsgType, Tokens0, Msg, File) ->
    ?debugVal(MsgType),
    case proplists:get_value(MsgType, ?HL7_TYPES) of
        undefined -> {ok, Msg};
        MsgDef0 ->
            ShortLen = erlang:min(length(MsgDef0), length(Tokens0)),
            {MsgDef, _} = lists:split(ShortLen, MsgDef0),
            {Tokens, _} = lists:split(ShortLen, Tokens0),
            %% ?debugVal( lists:zip(MsgDef, Tokens0)),
            Data0 = lists:map(fun({{Property, {maybe, _Type}, _Length, _Text}, ""}) ->
                                      %% ok to skip
                                      {Property, null};
                                 ({{Property, {maybe, Type}, Length, _Text}, Col}) ->
                                      ?debugVal({Property, Type}),
                                      to_json_object(Property, Type, Length, Col, 0);
                                 ({{Property, _Type, _Length, _Text}, ""}) ->
                                      %% warning
                                      io:format("[warning] empty property '~s' which isn't optional in ~s~n",
                                                [Property, File]),
                                      {Property, null};
                                      
                                 ({{Property, Type, Length, _Text}, Col}) ->
                                      ?debugVal({Property, Type}),
                                      to_json_object(Property, Type, Length, Col, 0)
                      end,
                      lists:zip(MsgDef, Tokens)),
            Data = lists:filter(fun({_,null}) -> false;
                                   (_) -> true end, Data0),

            case get_observation_value(Data) of
                undefined ->
                    {ok, append_segment(Msg, Data)};
                {'*', Col} -> %% special case for OBX-5
                    Data1 = case get_valuetype(Data) of
                                undefined ->
                                    delete_valuetype(Data);
                                ValueType ->
                                    Value = to_json_object(binary_to_atom(ValueType, latin1), Col, 1),
                                    [{<<"observation_value">>, Value}|delete_valuetype(Data)]
                            end,
                    {ok, append_segment(Msg, Data1)}
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
    

to_json_object(Property, Type, Len, Col, Depth) ->
    %% DAMN NONSENSE GUARD
    %% if length(Col) > Len ->
    %%         error({"too long text", Col, Len});
    %%    true ->
    {list_to_binary(Property), to_json_object(Type, Col, Depth)}.
%%end.

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
%to_json_object('FN', Col, _D)-> ?debugVal(Col), exit(1); %unicode:characters_to_binary(Col); %% undefined
to_json_object('SAD', Col, _D)-> unicode:characters_to_binary(Col); %% undefined
to_json_object('SPS', Col, _D)-> unicode:characters_to_binary(Col); %% undefined, maybe, and exists.
to_json_object('AUI', Col, _D)-> unicode:characters_to_binary(Col); %% undefined, maybe, and exists.
to_json_object('XCN', Col, _D)-> unicode:characters_to_binary(Col); %% broken data exists and less important

to_json_object(Name, Col, Depth)-> to_record(Name, Col, Depth).

get_separator(0) -> "[\\^]";
get_separator(1) -> "[~]";
get_separator(2) -> "[\\\\]";
get_separator(3) -> "[&]".

to_record(Name, Col, Depth) ->
    Tokens0 = re:split(Col, get_separator(Depth), [{return,list},unicode]),
    case proplists:get_value(Name, ?HL7_PRIMITIVE_TYPES) of
        undefined ->
            %% ?debugVal(Tokens0),
            %% io:put_chars(Col),
            error({unknown, Name});
        TypeDef0 ->
            ShortLen = erlang:min(length(TypeDef0), length(Tokens0)),
            {TypeDef, _} = lists:split(ShortLen, TypeDef0),
            {Tokens, _}  = lists:split(ShortLen, Tokens0),
            %% ?debugFmt("~", lists:zip(TypeDef, Tokens)),
            Data0 = lists:map(fun({{Property, _Type}, []}) -> {Property, null};
                                 ({{Property, Type}, Tok}) ->
                                      %% ?debugVal(Property),
                                      %% io:put_chars(Tok),
                                      %% ?debugVal(Type),
                                      {Property, to_json_object(Type, Tok, Depth+1)}
                              end,
                              lists:zip(TypeDef, Tokens)),
            Data = lists:filter(fun({_,null}) -> false;
                                   (_) -> true end, Data0),
            Data
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

-define(DECLEARE_TO_JSON(Segment),
        {Segment, record_info(fields, Segment)}
       ).

encoder() ->
    jsonx:encoder([{hl7msg, record_info(fields, hl7msg)}
                   %% ?DECLEARE_TO_JSON('MSH'),
                   %% ?DECLEARE_TO_JSON('EVN'),
                   %% ?DECLEARE_TO_JSON('PID'),
                   %% ?DECLEARE_TO_JSON('NK1'),
                   %% ?DECLEARE_TO_JSON('PV1'),
                   %% ?DECLEARE_TO_JSON('PV2'),
                   %% ?DECLEARE_TO_JSON('DB1'),
                   %% ?DECLEARE_TO_JSON('OBX'),
                   %% ?DECLEARE_TO_JSON('AL1'),
                   %% ?DECLEARE_TO_JSON('IN1'),
                   %% ?DECLEARE_TO_JSON('IAM'),
                   %% ?DECLEARE_TO_JSON('PRB'),
                   %% ?DECLEARE_TO_JSON('ZPR'),
                   %% ?DECLEARE_TO_JSON('ORC'),
                   %% ?DECLEARE_TO_JSON('TQ1'),
                   %% ?DECLEARE_TO_JSON('ODS'),
                   %% ?DECLEARE_TO_JSON('RXE'),
                   %% ?DECLEARE_TO_JSON('RXR'),
                   %% ?DECLEARE_TO_JSON('RXC'),
                   %% ?DECLEARE_TO_JSON('SPM'),
                   %% ?DECLEARE_TO_JSON('OBR')
                  ],
                  [{ignore, [null]}]).

to_json(#hl7msg{} = HL7Msg) ->
    E = encoder(),
    case E(HL7Msg) of
        Bin when is_binary(Bin) -> Bin;
        Other -> error(Other)
    end.
             
    %% lists:map(
    %%   fun(S) ->
    %%           ?debugVal("----------------------------------"),
    %%           ?debugVal(jsonx:encode(S)),
    %%           ?debugVal(S),
    %%           case proplists:get_value(<<"segid">>, S) of
    %%               <<"OBX">> ->
    %%                   lists:foreach(fun({Key,Value}) ->
    %%                                         ?debugVal({Key, jsonx:encode(Value)})
    %%                                 end, S),
    %%                   jsonx:encode(S);
    %%               _ ->
    %%                   ?debugVal(jsonx:encode(S)),
    %%                   jsonx:encode(S)
    %%           end
    %%   end,
    %%   HL7Msg#hl7msg.segments).
