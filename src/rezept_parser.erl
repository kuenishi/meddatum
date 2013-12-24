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
%% @doc
%%
%% @end
-module(rezept_parser).

-include("rezept.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([
         parse_file/3, parse_file/2,
         default_extractor/1
        ]).

-record(state, {recept       :: #recept{},
                records = [] :: list(),
                template     :: #recept{},
                mode         :: med | dpc}).

-spec parse_file(filename:filename(), med | dpc) -> {ok, [term()]}.
parse_file(Filename, Mode) ->
    parse_file(Filename, Mode, fun ?MODULE:default_extractor/1).

parse_file(Filename, Mode, InfoExtractor) when is_function(InfoExtractor) ->
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

    InitCtx = #state{template = #recept{file=BinFilename, checksum=BinChecksum},
                     mode = Mode},
    {ok, {Total, ResultCtx}} = ecsv:process_csv_string_with(lists:flatten(Lines), F, {0, InitCtx}),
    #state{recept=Recept, records=Records0} = ResultCtx,

    Records1 = case Recept of
                   undefined -> Records0;
                   _ when is_record(Recept, recept) ->
                       NewList = lists:reverse(Recept#recept.segments),
                       [Recept#recept{segments=NewList}|Records0]
               end,
    _ = lager:info("~p records for ~p lines.~n", [length(Records1), Total]),

    case InfoExtractor of
        undefined -> {ok, Records1};
        _  ->        {ok, lists:map(InfoExtractor, Records1)}
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

-spec gyymm2date(binary()) -> {ok, binary()} | {error, any()}.
gyymm2date(GYYMM) when is_binary(GYYMM) ->
    gyymm2date(binary_to_list(GYYMM));
gyymm2date([P,Y0,Y1,M0,M1]) ->
    Year = p2base(P) + list_to_integer([Y0,Y1]),
    {ok, iolist_to_binary([integer_to_list(Year) | [M0,M1]])};
gyymm2date(Other) ->
    {error, {bad_gyymm, Other}}.

-spec gyymmdd2date(binary()) -> {ok, binary()} | {error, any()}.
gyymmdd2date(GYYMMDD) when is_binary(GYYMMDD) ->
    gyymmdd2date(binary_to_list(GYYMMDD));
gyymmdd2date([P,Y0,Y1,M0,M1,D0,D1]) ->
    Year = p2base(P) + list_to_integer([Y0,Y1]),
    {ok, iolist_to_binary([integer_to_list(Year) | [M0,M1,D0,D1]])};
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

extract_patient_id(Col) ->
    proplists:get_value(kanjaid, Col).

%% how to make hardcoded "ほげほげ" printable:
hardcode_list_to_string(S) ->
    %%io:format(unicode:characters_to_list(list_to_binary(S))),
    S1 = list_to_binary(S),
    unicode:characters_to_list(S1).

parse_line(Line, LineNo, #state{recept=Recept, records=Records,
                                mode = Mode, template=ReceptTemplate} = State) ->
    [RecordID|_] = Line,

    Types = case Mode of
                dpc -> ?DPC_RECORD_TYPES;
                med -> ?MED_RECORD_TYPES
            end,

    case lists:keytake(RecordID, 1, Types) of
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
                    {ok, State#state{recept=undefined}};
                {"MN", true} -> %% error
                    {ok, State#state{recept=undefined,
                                     records=[rezept:finalize(Recept)|Records]}};
                {"IR", false} -> %% remember hospital ID
                    NewHospitalID = extract_hospital(Data),
                    Date = extract_ir_date(Data),
                    NewReceptTemplate = ReceptTemplate#recept{date=Date,
                                                              hospital_id=NewHospitalID},
                    {ok, State#state{recept=undefined, template=NewReceptTemplate}};
                {"IR", true} ->
                    NewHospitalID = extract_hospital(Data),
                    Date = extract_ir_date(Data),
                    NewReceptTemplate = ReceptTemplate#recept{date=Date,
                                                              hospital_id=NewHospitalID},
                    {ok, State#state{recept=undefined,
                                     records=[rezept:finalize(Recept)|Records],
                                     template=NewReceptTemplate}};
                {"GO", true} -> %% End of file
                    {ok, State#state{recept=undefined,
                                     records=[rezept:finalize(Recept)|Records]}};
                {"GO", false} -> %% End of file, buggy
                    {error, unexpected_eof};
                {"RE", true} ->
                    Date = extract_re_date(Data),
                    Recept2 = rezept:finalize(rezept:append_to_recept(Recept#recept{date=Date}, Data)),
                    NewRecept = ReceptTemplate#recept{patient_id=extract_patient_id(Data)},
                    {ok, State#state{recept=NewRecept, records=[Recept2|Records]}};
                {"RE", false} ->
                    Date = extract_re_date(Data),
                    NewRecept = rezept:append_to_recept(ReceptTemplate#recept{date=Date}, Data),
                    Recept2 = NewRecept#recept{patient_id=extract_patient_id(Data)},
                    {ok, State#state{recept=Recept2}};
                {_, true} ->
                    {ok, State#state{recept=rezept:append_to_recept(Recept, Data)}};
                {_, false} ->
                    {ok, State#state{recept=rezept:append_to_recept(ReceptTemplate, Data)}}
            end;
        {value, _, _} ->
            {error, {not_yet, RecordID}};
        false ->
            {error, {unknown_record, RecordID}}
    end.

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
