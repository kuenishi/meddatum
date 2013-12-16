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

-module(rezept).

-include_lib("eunit/include/eunit.hrl").
-include("rezept.hrl").

-export([process_file/2, process_file/3,
         parse_file/2, parse_file/3,
         default_extractor/1,
         from_json/1, to_json/1,
         key/1, key_prefix/1
        ]).

-spec process_file(filename:filename(), file:file_info()) -> {ok, [term()]}.
process_file(Filename, Info) ->
    process_file(Filename, Info, fun ?MODULE:default_extractor/1).

-spec process_file(filename:filename(), file:file_info(), fun()) -> {ok, [term()]}.
process_file(Filename, Info, InfoExtractor) when is_function(InfoExtractor) ->
    Records = parse_file(Filename, Info, InfoExtractor),
    {ok, Records}.

-spec parse_file(filename:filename(), file:file_info()) -> {ok, [term()]}.
parse_file(Filename, Info) ->
    parse_file(Filename, Info, fun ?MODULE:default_extractor/1).

parse_file(Filename, _Info, InfoExtractor) when is_function(InfoExtractor) ->
    {ok, Lines} = japanese:read_file(Filename),
    %% io:format("~p~n", [Lines]),
    _ = lager:info(Filename),
    {ok,Checksum} = checksum:file_md5(Filename),
    BinChecksum = checksum:bin_to_hexbin(Checksum),
    BinFilename = list_to_binary(lists:last(filename:split(Filename))),

    F = fun({newline, NewLine}, {LineNo, Ctx0}) ->
                {ok, Ctx} = parse_line(NewLine, LineNo, Ctx0),
                {LineNo+1, Ctx};
           (_, {LineNo, Ctx0}) -> {LineNo+1, Ctx0}
        end,

    InitCtx = {undefined, [], #recept{file=BinFilename, checksum=BinChecksum}},
    {ok, {Total, ResultCtx}} = ecsv:process_csv_string_with(lists:flatten(Lines), F, {0, InitCtx}),
    {Recept, Records0, _} = ResultCtx,

    Records1 = case Recept of
                   undefined -> Records0;
                   _ when is_record(Recept, recept) ->
                       NewList = lists:reverse(Recept#recept.segments),
                       [Recept#recept{segments=NewList}|Records0]
               end,
    _ = lager:info("~p records for ~p lines.~n", [length(Records1), Total]),

    case InfoExtractor of
        undefined -> Records1;
        _  ->        lists:map(InfoExtractor, Records1)
    end.

default_extractor(Record) -> Record.

extract_ir_date(Col) ->  extract_date(Col, seiym).
extract_re_date(Col) ->  extract_date(Col, shinym).

extract_date(Col, Key) ->
    proplists:get_value(Key, Col).
    %% case proplists:get_value(Key, Col) of
    %%     undefined -> undefined;
    %%     DateBin ->   DateBin
    %% end.

gyymm2date(GYYMM) when is_binary(GYYMM) ->
    gyymm2date(binary_to_list(GYYMM));
gyymm2date([P,Y0,Y1,M0,M1]) ->
    Year = p2base(P) + list_to_integer([Y0,Y1]),
    {ok, lists:flatten(integer_to_list(Year) ++ [M0,M1])};
gyymm2date(Other) ->
    {error, {bad_gyymm, Other}}.


gyymmdd2date(GYYMMDD) when is_binary(GYYMMDD) ->
    gyymmdd2date(binary_to_list(GYYMMDD));
gyymmdd2date([P,Y0,Y1,M0,M1,D0,D1]) ->
    Year = p2base(P) + list_to_integer([Y0,Y1]),
    {ok, lists:flatten(integer_to_list(Year) ++ [M0,M1,D0,D1])};
gyymmdd2date(Other) ->
    {error, {bad_gyymmdd, Other}}.


p2base($4) -> 1988; %% 平成0年
p2base($3) -> 1925; %% 昭和0年
p2base($2) -> 1911; %% 大正0年
p2base($1) -> 1971. %% 明治0年

%% TODO: write tests
extract_hospital(Col) ->
    PrefID = proplists:get_value(state, Col),
    HospID = proplists:get_value(cocd, Col),
    re:replace(lists:flatten(io_lib:format("~.2w~.7w", [PrefID,HospID])),
               " ", "0", [{return,binary},global]).

encoder() ->
    jsonx:encoder([{recept, record_info(fields, recept)}],
                  [{ignore, [null]}]).

decoder() ->
    jsonx:decoder([{recept, record_info(fields, recept)}],
                  [{ignore, [null]}]).

-define(ENCODER, (encoder())).
-define(DECODER, (decoder())).

%% maybe_taken(0, List) -> List;
%% maybe_taken(_, []) -> [];
%% maybe_taken(N, List) ->
%%     maybe_taken(N-1, tl(List)).

-spec to_json(#recept{}) -> {ok, binary()}.
to_json(Rezept) when is_record(Rezept, recept) > 0 ->

    case ?ENCODER(Rezept) of
        {error, A, B} ->
            _ = lager:error("to_json: ~p, ~p~n", [A, B]),
            {error, {A,B}};
        {no_match, _O} ->
            _ = lager:error("~p~n", [Rezept]),
            {error, no_match};
        JSONRecords when is_binary(JSONRecords) ->
            {ok, JSONRecords}
    end;
to_json(_R) ->
    {error, empty}.

-spec from_json(JSON::binary()) -> #recept{}.
from_json(RezeptJson) ->
    ?DECODER(RezeptJson).

-spec key(#recept{}) -> binary().
key(#recept{file=undefined}) ->   error(no_file_specified);
key( #recept{file=Filename, checksum=undefined} = Recept) ->
    {ok, Checksum} = checksum:file_md5(Filename),
    BinChecksum = checksum:bin_to_hexbin(Checksum),
    key(Recept#recept{checksum=BinChecksum});
key( #recept{file=BinFilename, checksum=Checksum} = Recept) ->
    Hash = integer_to_binary(erlang:phash2(Recept)),
    <<BinFilename/binary, "-", Checksum/binary, "-", Hash/binary>>.

-spec key_prefix(filename:filename()) -> binary().
key_prefix(Filename) when is_list(Filename) ->
    BinFilename = list_to_binary(lists:last(filename:split(Filename))),
    {ok,Checksum} = checksum:file_md5(Filename),
    BinChecksum = checksum:bin_to_hexbin(Checksum),
    <<BinFilename/binary, "-", BinChecksum/binary>>.

%% how to make hardcoded "ほげほげ" printable:
hardcode_list_to_string(S) ->
    %%io:format(unicode:characters_to_list(list_to_binary(S))),
    S1 = list_to_binary(S),
    unicode:characters_to_list(S1).

parse_line(Line, LineNo, {Recept, Records, ReceptTemplate}) ->
    [RecordID|_] = Line,

    case lists:keytake(RecordID, 1, ?RECORD_TYPES) of
        {value, {RecordID, Name, Cols0}, _} ->
            RecordType = hardcode_list_to_string(Name),
            _ = lager:debug("[~s] ~ts", [RecordID, RecordType]),

            ShortLen = erlang:min(length(Line), length(Cols0)),
            {Line1,_} = lists:split(ShortLen, Line),
            {Cols, _} = lists:split(ShortLen, Cols0),
            %% case LineNo of
            %%     N when N < 1185 andalso N > 1180 ->
            %%         ?debugFmt("~w", [lists:zip(Cols, Line1)]);
            %%     N when N < 1181 -> ok;
            %%     _ ->
            %%         exit(normal)
            %% end,

            Data0 = lists:map(fun({Col, Entry}) ->
                                      %%?debugVal({Col,Entry}),
                                      case check_type(Col, Entry) of
                                          {ok, {K,V}} ->
                                              {K, V};
                                          {warning, {error, O}} ->
                                              _ = lager:warning("Line ~p: ~p",[LineNo, O]),
                                              {col, null};

                                          {warning, {K,null}} ->
                                              %% _ = lager:warning("required value is empty at ~s: ~ts",
                                              %%                   [RecordID, hardcode_list_to_string(K)]),
                                              {K, null};
                                          {warning, {K,V}} ->
                                              {K, V}
                                      end
                              end,
                              lists:zip(Cols, Line1)),
            Data = lists:filter(fun({_,null}) -> false; (_) -> true end,
                                Data0),

            case {RecordID, is_record(Recept, recept)} of
                {"MN", false} -> %% skip
                    {ok, {undefined, Records, ReceptTemplate}};
                {"MN", true} -> %% error
                    {ok, {undefined, [finalize_recept(Recept)|Records], ReceptTemplate}};
                {"IR", false} -> %% remember hospital ID
                    NewHospitalID = extract_hospital(Data),
                    Date = list_to_binary(extract_ir_date(Data)),
                    NewReceptTemplate = ReceptTemplate#recept{date=Date,
                                                              hospital_id=NewHospitalID},
                    {ok, {undefined, Records, NewReceptTemplate}};
                {"IR", true} ->
                    NewHospitalID = extract_hospital(Data),
                    Date = list_to_binary(extract_ir_date(Data)),
                    NewReceptTemplate = ReceptTemplate#recept{date=Date,
                                                              hospital_id=NewHospitalID},
                    {ok, {undefined, [finalize_recept(Recept)|Records], NewReceptTemplate}};
                {"GO", true} -> %% End of file
                    {ok, {undefined, [finalize_recept(Recept)|Records], ReceptTemplate}};
                {"GO", false} -> %% End of file, buggy
                    {error, unexpected_eof};
                {"RE", true} ->
                    Date = list_to_binary(extract_re_date(Data)),
                    Recept2 = finalize_recept(append_to_recept(Recept#recept{date=Date}, Data)),
                    {ok, {ReceptTemplate, [Recept2|Records], ReceptTemplate}};
                {"RE", false} ->
                    Date = list_to_binary(extract_re_date(Data)),
                    Recept2 = append_to_recept(ReceptTemplate#recept{date=Date}, Data),
                    {ok, {Recept2, Records, ReceptTemplate}};
                {_, true} ->
                    {ok, {append_to_recept(Recept, Data), Records, ReceptTemplate}};
                {_, false} ->
                    {ok, {append_to_recept(ReceptTemplate, Data), Records, ReceptTemplate}}
            end;
        {value, _, _} ->
            {error, {not_yet, RecordID}};
        false ->
            {error, {unknown_record, RecordID}}
    end.

append_to_recept(#recept{segments=List} = Recept, Data) ->
    Recept#recept{segments=[Data|List]}.

finalize_recept(#recept{segments=List} = Recept) ->
    NewList = lists:reverse(List),
    Recept#recept{segments=NewList}.

-spec check_type({atom(), atom()|{maybe,atom()}, integer()}, string())
                -> {ok, {string(), null|binary()}}. %% unicode binary
check_type({Name, {maybe, _}, _}, []) -> {ok, {Name, null}};
check_type({Name, {maybe, Type}, MaxDigits}, Entry) ->
    check_type({Name, Type, MaxDigits}, Entry);
check_type({Name, _, _}, []) ->
    %% required field is null
    {warning, {Name, null}};
check_type({Name, integer, _MaxDigits}, Entry) ->
    {ok, {Name, list_to_integer(Entry)}};
check_type({Name, latin1, _MaxDigits}, Entry) ->
    {ok, {Name, list_to_binary(Entry)}};
check_type({Name, unicode, _MaxDigits}, Entry) ->
    %% meddatum:log(">>>> => ~ts~n", [unicode:characters_to_binary(Entry, utf8)]),
    {ok, {Name, unicode:characters_to_binary(Entry, unicode)}};
check_type({Name, gyymm, 5}, Entry) ->
    case gyymm2date(Entry) of
        {ok, Date} -> {ok, {Name, Date}};
        {error, O} -> {warning, {error, O}}
    end;

check_type({Name, gyymmdd, 7}, Entry) ->
    case gyymmdd2date(Entry) of
        {ok, Date} -> {ok, {Name, Date}};
        {error, O} -> {warning, {error, O}}
    end;

check_type({Name, jy_code, _}, Entry) when length(Entry) =:= 1 ->
    {ok, {Name, unicode:characters_to_binary(Entry)}}.


-ifdef(TEST).

%% TODO: write tests
extract_hospital_test() ->
    Col = [{state, 1}, {cocd, 1}],
    PrefID = proplists:get_value(state, Col),
    HospID = proplists:get_value(cocd, Col),
    Str = re:replace(lists:flatten(io_lib:format("~.2w~.7w", [PrefID,HospID])),
                     " ", "0", [{return,binary},global]),
    ?assertEqual(<<"010000001">>, Str).


-endif.
