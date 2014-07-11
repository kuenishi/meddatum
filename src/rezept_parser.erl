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
         default_extractor/1,
         postprocess/2
        ]).

-record(state, {recept       :: #recept{},
                records = [] :: list(),
                template     :: #recept{},
                mode         :: med | dpc,
                skipping = false :: boolean(),
                filename     :: filename:filename()
               }).

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
           ({eof}, {LineNo, Ctx0}) ->
                {ok, Ctx} = handle_eof(LineNo, Ctx0),
                {LineNo+1, Ctx};
           (_, {LineNo, Ctx0}) -> {LineNo+1, Ctx0}
        end,

    InitCtx = #state{template = #recept{file=BinFilename, checksum=BinChecksum},
                     filename = BinFilename, mode = Mode},
    {ok, {Total, ResultCtx}} = ecsv:process_csv_string_with(lists:flatten(Lines), F, {0, InitCtx}),

    #state{records=Records1} = ResultCtx,
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

parse_line(Line, LineNo, #state{mode = Mode, filename = Filename} = State) ->

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
            try
                handle_split_line(LineNo, Cols, Line1, RecordID, State)
            catch
                T:E ->
                    _ = lager:error("Parse error at ~p@~s (~p:~p). Skipping until next RE.",
                                    [LineNo, Filename, T, E]),
                    {ok, State#state{skipping=true}}
            end;
        {value, _, _} ->
            {error, {not_yet, RecordID}};
        false ->
            {error, {unknown_record, RecordID}}
    end.

handle_split_line(LineNo, Cols, Line1, RecordID,
                  #state{recept=Recept, records=Records,
                         template=ReceptTemplate,
                         skipping = Skipping} = State) ->

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
    Data1 = lists:filter(fun({_,null}) -> false; (_) -> true end,
                         Data0),
    Data = [{<<"l">>,LineNo}|Data1],

    case {RecordID, is_record(Recept, recept)} of
        {"MN", false} -> %% skip
            {ok, State#state{recept=undefined}};
        {"MN", true} -> %% error
            {ok, State#state{recept=undefined,
                             records=[rezept:finalize(Recept)|Records]}};
        {"IR", _} -> %% remember hospital ID
            NewRecords = case Recept of
                             undefined -> Records;
                             _ when is_record(Recept, recept) -> [rezept:finalize(Recept)|Records]
                         end,
            NewHospitalID = extract_hospital(Data),
            Date = extract_ir_date(Data),
            NewReceptTemplate = ReceptTemplate#recept{date=Date,
                                                      hospital_id=NewHospitalID},
            {ok, State#state{recept=undefined,
                             records=NewRecords,
                             template=NewReceptTemplate}};
        {"GO", true} -> %% End of file
            {ok, State#state{recept=undefined,
                             records=[rezept:finalize(Recept)|Records]}};
        {"GO", false} -> %% End of file, buggy
            {error, unexpected_eof};
        {"RE", _} ->
            NewRecords = case Recept of
                             undefined -> Records;
                             _ when Skipping -> Records;
                             _ when is_record(Recept, recept) -> [rezept:finalize(Recept)|Records]
                         end,
            NewRecept0 = ReceptTemplate#recept{patient_id=extract_patient_id(Data),
                                               date=extract_re_date(Data)},
            NewRecept = rezept:append_to_recept(NewRecept0, Data),
            {ok, State#state{recept=NewRecept, records=NewRecords, skipping=false}};

        {_, true} when Skipping ->
            {ok, State};
        {_, true} ->
            {ok, State#state{recept=rezept:append_to_recept(Recept, Data)}};
        {_, false} ->
            {ok, State#state{recept=rezept:append_to_recept(ReceptTemplate, Data)}}
    end.


handle_eof(_LineNo, #state{recept=undefined} = Ctx0) ->
    {ok, Ctx0};
handle_eof(_LineNo, #state{recept=Recept0, records=Records} = Ctx0) ->
    Recept = rezept:finalize(Recept0),
    {ok, Ctx0#state{recept=undefined, records=[Recept|Records]}}.

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

%% @doc actual postprocessor of each recept record
postprocess(Seg, #recept{date=Date} = _Recept) ->
    <<YYYYMM:6/binary, _/binary>> = Date,
    case proplists:get_value(record_info, Seg) of
         <<"SI">> -> handle_30days(Seg, YYYYMM);
         <<"IY">> -> handle_30days(Seg, YYYYMM);
         <<"TO">> -> handle_30days(Seg, YYYYMM);
         _ -> Seg
    end.

%% @private
handle_30days(Seg, Date) ->
    {NewSeg, History} = handle_30days(Seg, Date, [], []),
    lists:reverse([{history,History}|NewSeg]).

handle_30days([], _, NewSeg, History) -> {NewSeg, lists:reverse(History)};
handle_30days([{info_1, V}|TL], Date, NewSeg, History) -> handle_30days(TL, Date, NewSeg, [day(<<"01">>, V, Date)|History]);
handle_30days([{info_2, V}|TL], Date, NewSeg, History) -> handle_30days(TL, Date, NewSeg, [day(<<"02">>, V, Date)|History]);
handle_30days([{info_3, V}|TL], Date, NewSeg, History) -> handle_30days(TL, Date, NewSeg, [day(<<"03">>, V, Date)|History]);
handle_30days([{info_4, V}|TL], Date, NewSeg, History) -> handle_30days(TL, Date, NewSeg, [day(<<"04">>, V, Date)|History]);
handle_30days([{info_5, V}|TL], Date, NewSeg, History) -> handle_30days(TL, Date, NewSeg, [day(<<"05">>, V, Date)|History]);
handle_30days([{info_6, V}|TL], Date, NewSeg, History) -> handle_30days(TL, Date, NewSeg, [day(<<"06">>, V, Date)|History]);
handle_30days([{info_7, V}|TL], Date, NewSeg, History) -> handle_30days(TL, Date, NewSeg, [day(<<"07">>, V, Date)|History]);
handle_30days([{info_8, V}|TL], Date, NewSeg, History) -> handle_30days(TL, Date, NewSeg, [day(<<"08">>, V, Date)|History]);
handle_30days([{info_9, V}|TL], Date, NewSeg, History) -> handle_30days(TL, Date, NewSeg, [day(<<"09">>, V, Date)|History]);
handle_30days([{info_10, V}|TL], Date, NewSeg, History) -> handle_30days(TL, Date, NewSeg, [day(<<"10">>, V, Date)|History]);
handle_30days([{info_11, V}|TL], Date, NewSeg, History) -> handle_30days(TL, Date, NewSeg, [day(<<"11">>, V, Date)|History]);
handle_30days([{info_12, V}|TL], Date, NewSeg, History) -> handle_30days(TL, Date, NewSeg, [day(<<"12">>, V, Date)|History]);
handle_30days([{info_13, V}|TL], Date, NewSeg, History) -> handle_30days(TL, Date, NewSeg, [day(<<"13">>, V, Date)|History]);
handle_30days([{info_14, V}|TL], Date, NewSeg, History) -> handle_30days(TL, Date, NewSeg, [day(<<"14">>, V, Date)|History]);
handle_30days([{info_15, V}|TL], Date, NewSeg, History) -> handle_30days(TL, Date, NewSeg, [day(<<"15">>, V, Date)|History]);
handle_30days([{info_16, V}|TL], Date, NewSeg, History) -> handle_30days(TL, Date, NewSeg, [day(<<"16">>, V, Date)|History]);
handle_30days([{info_17, V}|TL], Date, NewSeg, History) -> handle_30days(TL, Date, NewSeg, [day(<<"17">>, V, Date)|History]);
handle_30days([{info_18, V}|TL], Date, NewSeg, History) -> handle_30days(TL, Date, NewSeg, [day(<<"18">>, V, Date)|History]);
handle_30days([{info_19, V}|TL], Date, NewSeg, History) -> handle_30days(TL, Date, NewSeg, [day(<<"19">>, V, Date)|History]);
handle_30days([{info_20, V}|TL], Date, NewSeg, History) -> handle_30days(TL, Date, NewSeg, [day(<<"20">>, V, Date)|History]);
handle_30days([{info_21, V}|TL], Date, NewSeg, History) -> handle_30days(TL, Date, NewSeg, [day(<<"21">>, V, Date)|History]);
handle_30days([{info_22, V}|TL], Date, NewSeg, History) -> handle_30days(TL, Date, NewSeg, [day(<<"22">>, V, Date)|History]);
handle_30days([{info_23, V}|TL], Date, NewSeg, History) -> handle_30days(TL, Date, NewSeg, [day(<<"23">>, V, Date)|History]);
handle_30days([{info_24, V}|TL], Date, NewSeg, History) -> handle_30days(TL, Date, NewSeg, [day(<<"24">>, V, Date)|History]);
handle_30days([{info_25, V}|TL], Date, NewSeg, History) -> handle_30days(TL, Date, NewSeg, [day(<<"25">>, V, Date)|History]);
handle_30days([{info_26, V}|TL], Date, NewSeg, History) -> handle_30days(TL, Date, NewSeg, [day(<<"26">>, V, Date)|History]);
handle_30days([{info_27, V}|TL], Date, NewSeg, History) -> handle_30days(TL, Date, NewSeg, [day(<<"27">>, V, Date)|History]);
handle_30days([{info_28, V}|TL], Date, NewSeg, History) -> handle_30days(TL, Date, NewSeg, [day(<<"28">>, V, Date)|History]);
handle_30days([{info_29, V}|TL], Date, NewSeg, History) -> handle_30days(TL, Date, NewSeg, [day(<<"29">>, V, Date)|History]);
handle_30days([{info_30, V}|TL], Date, NewSeg, History) -> handle_30days(TL, Date, NewSeg, [day(<<"30">>, V, Date)|History]);
handle_30days([{info_31, V}|TL], Date, NewSeg, History) -> handle_30days(TL, Date, NewSeg, [day(<<"31">>, V, Date)|History]);
handle_30days([HD|TL], Date, NewSeg, History) -> handle_30days(TL, Date, [HD|NewSeg], History).

day(Day, V, Date) ->
    [{date, <<Date/binary, Day/binary>>}, {cnt, V}].

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
