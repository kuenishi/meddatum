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

-module(episode_extractor).

-include_lib("eunit/include/eunit.hrl").
-include_lib("meddatum/include/hl7.hrl").
-include_lib("meddatum/include/rezept.hrl").
-include_lib("meddatum/include/meddatum.hrl").
-include_lib("meddatum/include/episode.hrl").

-ifdef(TEST).
-export([process_episodes/5, get_endpoints/1]).
-endif.

-export([process_a_patient/4]).

-spec process_a_patient([{binary(),binary(),binary()}], pid(),
                        string(), filename:filename()) -> [ok].
process_a_patient(RiakKeys, Riakc, Margin, Dir) ->
    {ok, {HospitalID, PatientID, BKeys}} = check_keys(RiakKeys),
    case PatientID of
        %% PatientID can be undefined in case of '*:*' query
        %% requested so drop them all.
        undefined ->

            %% TODO: output logs here.
            {error, bad_data};
        _ ->
            io:format("fetched: gonna process ~p~n", [PatientID]),
            lager:debug("found keys: ~p~n", [BKeys]),

            {ok, SSMIXResults} =
                get_all_via_index(Riakc,
                                  hl7:bucket(normal),
                                  PatientID),
            {ok, ReceptResults} =
                get_all_via_index(Riakc,
                                  rezept:bucket(boom),
                                  PatientID),
            {ok, StaticRecords0} =
                get_all_via_index(Riakc,
                                  hl7:bucket(static),
                                  PatientID),
            {ok, SearchResults} = get_all_keys(Riakc, BKeys),

            %% TODO: to improve performance, make these
            %% mapred happen on server side.
            HL7list = sort_by_date(json_to_terms(ssmix, SSMIXResults)),
            Receptlist = sort_by_date(json_to_terms(rezept, ReceptResults)),
            StaticRecords = json_to_terms(ssmix, StaticRecords0),
            HitDates = extract_date_from_json(SearchResults),

            Got = io_lib:format("~p hl7, ~p recept, ~p dates",
                                [length(HL7list), length(Receptlist),
                                 length(HitDates)]),

            Episodes = process_episodes(PatientID, HitDates,
                                        HL7list,
                                        Receptlist, Margin),

            lager:info("~p episodes (~s) found on patient ~p @ ~p",
                       [length(Episodes), Got, PatientID,
                        HospitalID]),

            episode:save_records(PatientID, HospitalID,
                                 StaticRecords, Dir),
            [ episode:save(PatientID, I, Episode, Recepts, Dir)
              || {I, Episode, Recepts} <- Episodes ]
    end.

%% @private
-spec process_episodes(PatientID::binary(), [], [], [],
                       string()) -> Episodes::[{non_neg_integer(),
                                                #episode{}, list()
                                               }].
process_episodes(<<"undefined">>, _, _, _, _) -> [];
process_episodes(_, [], [], [], _) -> [];
process_episodes(PatientID, HitDates, HL7list, Receptlist, Margin) ->

    RawEndpoints = get_endpoints(HL7list),
    lager:debug("RawEndpoints: ~p", [RawEndpoints]),

    %% If the search result includes any static SSMIX file,
    %% All episodes are to be exported.
    EpisodeEndpoints =
        case lists:any(fun hl7:is_static/1, HL7list) of
            true -> RawEndpoints;
            _    -> filter_endpoints(RawEndpoints, HitDates)
        end,
    lager:debug("FilterdEndpoints: ~p (~p)", [EpisodeEndpoints, HitDates]),

    io:format("[info] ~p episode endpoints found on patient ~p.~n",
              [length(EpisodeEndpoints), PatientID]),
    io:format("[debug] episode endpoints: ~p.~n", [EpisodeEndpoints]),

    Episodes = [ episode:new(Admission, Discharge, HL7list, Margin)
                 || {Admission, Discharge} <- EpisodeEndpoints ],

    ReceptEpisodes =
        [filter_recept_list(Start, End, Receptlist)
          || #episode{start_date=Start, end_date=End} <- Episodes],

    lists:zip3(lists:seq(1, length(Episodes)),
               Episodes, ReceptEpisodes).

filter_recept_list(Start0, End0, ReceptList) ->
    <<Start:6/binary, _/binary>> = Start0,
    <<End:6/binary, _/binary>> = End0,
    lists:filter(fun(Recept) ->
                         Date = episode:extract_date(Recept),
                         (Start =< Date) andalso (Date =< End)
                 end, ReceptList).


%% @private
-spec get_all_via_index(pid(), binary(), binary()) -> list().
get_all_via_index(Riakc, Bucket, PatientID) ->
    MapRedQuery = [{map, {modfun, riak_kv_mapreduce, map_object_value},
                    <<"filter_notfound">>, false},
                   {reduce, {modfun, riak_kv_mapreduce, reduce_sort},
                    undefined, true}],
    MapRedRange =  {index, Bucket, <<"patient_id_bin">>, PatientID},
    riakc_pb_socket:mapred(Riakc, MapRedRange, MapRedQuery).

%% @private
-spec get_all_keys(pid(), [{binary(),binary()}]) -> list().
get_all_keys(Riakc, BKs) ->
    MapRedQuery = [{map, {modfun, riak_kv_mapreduce, map_object_value},
                    <<"filter_notfound">>, false},
                   {reduce, {modfun, riak_kv_mapreduce, reduce_sort},
                    undefined, true}],
    riakc_pb_socket:mapred(Riakc, BKs, MapRedQuery).
    %% Result = lists:map(fun({B,K}) ->
    %%                            {ok, Obj0} = riakc_pb_socket:get(Riakc,B,K),
    %%                            riakc_obj:get_value(Obj0)
    %%                    end, BKs),
    %% %% This sort happens probably because first element of the tuple "date":"2023..."
    %% %% is sorted here.
    %% {ok, [{1, lists:sort(Result)}]}.

%% @private
extract_date_from_json(MapRedResults) ->

    GetDate = fun(JSON) ->
                      {Proplist} = jsone:decode(JSON),
                      Date = proplists:get_value(<<"date">>, Proplist),
                      Date
              end,
    lists:flatten([ lists:map(GetDate, JSONList)
                    || {_,JSONList} <- MapRedResults ]).

%% @private
%% TODO: make this selective ...
%% current:  if no endpoints extracted then
%% return whole data as a single episode.
-spec filter_endpoints([{binary(),binary()}], [binary()]) -> [{binary(),binary()}].
filter_endpoints([], _) ->
    [{?BEGINNING_OF_THE_WORLD, ?END_OF_THE_WORLD}];
filter_endpoints(Endpoints, HitDates) ->

    Filter = fun({Admission, Discharge}) ->
                     lists:any(fun(Date) when byte_size(Date) >= 8 ->
                                       %% ssmix
                                       %% ?debugVal({Admission, Date, Discharge}),
                                       %% ?debugVal((Admission =< Date)
                                       %%          andalso (Date =< Discharge)),
                                       (Admission =< Date)
                                           andalso
                                             (Date =< Discharge);

                                  (Date) when byte_size(Date) =:= 6 ->
                                       %% recept
                                       <<Start:6/binary, _/binary>> = Admission,
                                       <<End:6/binary, _/binary>> = Discharge,
                                       %%?debugVal({Start, Date, End}),
                                       %%?debugVal((Start =< Date) andalso (Date =< End)),
                                       (Start =< Date)
                                           andalso
                                             (Date =< End);

                                  (<<"-">>) ->
                                       %% ssmix-patient record does not have date
                                       false
                               end, HitDates)
             end,
    lists:filter(Filter, Endpoints).

%% @private
sort_by_date(HL7List0) ->
    List0 = [{episode:extract_date(HL7orRecept), HL7orRecept}
             || HL7orRecept <- HL7List0],
    List1 = lists:sort(List0),
    [HL70 || {_, HL70} <- List1].

%% @private
%% note: this code may cause problem if there are open-period episodes
%% before everything starts, because there may be a chance that dischage
%% date can be earlier than admission. There are an assumption that
%% the periods are complete and corresponding admission always exists.
-spec get_endpoints([#hl7msg{}]) -> [{binary(),binary()}].
get_endpoints(HL7List) ->
    L = lists:foldl(fun(HL7Msg, Acc) ->
                            endpoint_extract_folder(HL7Msg, Acc)
                    end, [], HL7List),

    %% reverse order of
    %% [{admission, date}, {discharge, date}, {admission, .... ]
    E = sort_endpoints(L),

    Episodes = not_in_hosp(E,
                           ?BEGINNING_OF_THE_WORLD, []),
    E2 = klib:rev_map(fun({<<Admission:8/binary, _/binary>>,
                          <<Discharge:8/binary, _/binary>>}) -> {Admission, Discharge}
                     end, Episodes),
    E2.

-spec sort_endpoints([{admission|discharge,binary()}]) -> [{admission|discharge,binary()}].
sort_endpoints(Endpoints) ->
    %% sort by date, regardless of admission/discharge
    Sorter = fun({_, LDate}, {_, RDate}) -> LDate =< RDate end,
    Folder = fun({admission, Date}, [{Date,A,D}|Acc0]) -> [{Date,A+1,D}|Acc0];
                ({discharge, Date}, [{Date,A,D}|Acc0]) -> [{Date,A,D+1}|Acc0];
                ({admission, Date}, Acc0) -> [{Date,1,0}|Acc0];
                ({discharge, Date}, Acc0) -> [{Date,0,1}|Acc0]
             end,
    ByDate = lists:foldl(Folder, [], lists:sort(Sorter, Endpoints)),
    _BySeq = sort_by_seq(ByDate, []).

sort_by_seq([], Acc) -> Acc;
sort_by_seq([{Date, A, D}|TL], [{admission, Date}|_] = Acc) when D > 0 ->
    Next = [{discharge, Date}|Acc],
    case {A,D} of
        {0, 1} -> sort_by_seq(TL, Next);
        _ ->      sort_by_seq([{Date, A, D-1}|TL], Next)
    end;
sort_by_seq([{Date, A, D}|TL], [{discharge, Date}|_] = Acc) when A > 0 ->
    Next = [{admission, Date}|Acc],
    case {A, D} of
        {1, 0} -> sort_by_seq(TL, Next);
        _ ->      sort_by_seq([{Date, A-1, D}|TL], Next)
    end;
sort_by_seq([{Date, A, D}|TL], Acc) ->
    case {A,D} of
        {0, 1} -> sort_by_seq(TL, [{discharge, Date}|Acc]);
        {1, 0} -> sort_by_seq(TL, [{admission, Date}|Acc]);
        {A, D} when A >= D -> sort_by_seq([{Date, A-1, D}|TL], [{admission, Date}|Acc]);
        {A, D} when A < D -> sort_by_seq([{Date, A, D-1}|TL], [{discharge, Date}|Acc])
    end.

%% @private
endpoint_extract_folder(HL7msg, Acc0) ->
    MsgType = string:left(hl7:msg_type(HL7msg), 7),
    case {hl7:get_segment(HL7msg, <<"PV1">>), MsgType} of
        {{ok, Segment}, "ADT^A01"} ->
            case proplists:get_value(<<"nyudate">>, Segment) of
                undefined ->  Acc0;
                Admission0 ->
                    [{admission, extract_datetime(Admission0)}|Acc0]
            end;
        {{ok, Segment}, "ADT^A03"} ->
            case proplists:get_value(<<"taidate">>, Segment) of
                undefined ->  Acc0;
                Discharge0 ->
                    [{discharge, extract_datetime(Discharge0)}|Acc0]
            end;
        _ -> Acc0
    end.

%% @private
not_in_hosp([{admission, Date}|TL], PrevDate, E) ->
    in_hosp(TL, Date, [{PrevDate, Date}|E]);
not_in_hosp([{discharge, Date}|TL], PrevDate, E) ->
    %% probably already in hospital!
    not_in_hosp(TL, Date, [{PrevDate, Date}|E]);
not_in_hosp([], PrevDate, E) ->
    [{PrevDate, ?END_OF_THE_WORLD}|E].

%% @private
in_hosp([{admission, Date}|TL], _PrevDate, E) ->
    %% unexpected, ignored
    in_hosp(TL, Date, E);
in_hosp([{discharge, Date}|TL], PrevDate, E) ->
    not_in_hosp(TL, Date, [{PrevDate, Date}|E]);
in_hosp([], PrevDate, E) ->
    [{PrevDate, ?END_OF_THE_WORLD}|E].

%% @private
extract_datetime({Proplist}) ->
    proplists:get_value(<<"time">>, Proplist).

%% @private
json_to_terms(ssmix, Results) ->
    fold_results(fun hl7:from_json/1, fun(V, Acc0) -> [V|Acc0] end, [], Results);
json_to_terms(rezept, Results) ->
    fold_results(fun rezept:from_json/1, fun(V, Acc0) -> [V|Acc0] end, [], Results).

%% @private
fold_json_bins(_, _, Acc, []) -> Acc;
fold_json_bins(DecodeFun, FoldFun, Acc0, [H|L]) ->
    Acc = FoldFun(DecodeFun(H), Acc0),
    fold_json_bins(DecodeFun, FoldFun, Acc, L).

%% @doc
%% @private
fold_results(_, _, Acc, []) -> Acc;
fold_results(DecodeFun, FoldFun, Acc0, [{_Int,List}|Remain]) ->
    Acc = fold_json_bins(DecodeFun, FoldFun, Acc0, List),
    fold_results(DecodeFun, FoldFun, Acc, Remain).

%% @doc
%% @private
-spec check_keys([{{binary(), binary()},{binary(),binary()},binary()}]) -> {ok, {binary(), [{binary(),binary()}]}}.
check_keys(RiakKeys) ->
    {{HospitalID, PatientID}, B, K} = hd(RiakKeys),
    {ok, Bkeys} = check_keys(tl(RiakKeys), [{{B,K}, undefined}], PatientID),
    {ok, {HospitalID, PatientID, Bkeys}}.

-spec check_keys([{binary(),binary(),binary()}],
                 [{binary(), binary()}], binary()) ->
                        {ok, {binary(), [{binary(),binary()}]}}.
check_keys([], Bkeys, _) -> {ok, Bkeys};
check_keys([{{_,PatientID},B,K}|TL], Bkeys, PatientID) ->
    BK = {{B,K}, undefined},
    check_keys(TL, [BK|Bkeys], PatientID).

-ifdef(TEST).

build_hl7msg(PatientID, Date0, Type) ->
    Date = {[{<<"time">>, Date0}]},
    PV1= case Type of
             admission -> [{<<"nyudate">>, Date},
                           {<<"segid">>, <<"PV1">>}];
             discharge -> [{<<"taidate">>, Date},
                           {<<"segid">>, <<"PV1">>}];
             other ->     []
         end,
    #hl7msg{patient_id= PatientID,
            date=Date0,
            msg_type_s=
                case Type of
                    admission -> <<"ADT^A01">>;
                    discharge -> <<"ADT^A03">>;
                    other -> <<"otherrrrr">>
                end,
            segments=[{PV1}]}.

build_recept(PatientID, Date) ->
    #recept{patient_id=PatientID,
            date=Date}.

episode_extraction_from_recept_test() ->
    PatientID = <<"simpsons">>,

    HL7List =
        lists:map(fun({Date, MsgType}) ->
                          build_hl7msg(PatientID, Date, MsgType)
                  end,
                  [
                   {<<"20120522">>, admission},
                   {<<"20120523">>, other},
                   {<<"20120628">>, discharge}]),

    ReceptList = [build_recept(PatientID, <<"201205">>)],
    HitDates = [<<"201205">>] ++ [ Date || #hl7msg{date=Date} <- HL7List ],
    %% ?debugVal(HitDates),
    Endpoints = episode_extractor:get_endpoints(HL7List),
    ?assert(length(Endpoints) > 0),
    Episodes0 = episode_extractor:process_episodes(
                 PatientID, HitDates, HL7List, ReceptList, ""),
    ?assert(length(Episodes0) > 0),

    Episodes = [Episode || {_, Episode, _} <- Episodes0],

    StartsEnds = [ {Start, End} ||
                     #episode{start_date = Start,
                              end_date = End} <- Episodes ],

    ?assertEqual([{<<"19000101">>,<<"20120522">>},
                  {<<"20120522">>,<<"20120628">>},
                  {<<"20120628">>,<<"30001231">>}], StartsEnds),

    %% ?debugFmt("~p", [Episodes0]),
    [{1,First,ReceptList},
     {2,Second,ReceptList},
     {3,Third,[]}] = Episodes0,
    ?assertEqual(1, length(First#episode.hl7_msgs)),
    ?assertEqual(3, length(Second#episode.hl7_msgs)),
    ?assertEqual(1, length(Third#episode.hl7_msgs)).

episode_extraction_test() ->
    PatientID = <<"simpsons">>,
    %% この場合、*の日にADT^A01が、#の日にADT^A03があります。
    %% 2011/12/20 外来
    %% 2012/01/10 入院 *
    %% 2012/01/15 退院 #
    %% 2012/01/16 入院 *
    %% 2012/01/18 退院 #
    %% 2012/01/18 入院 *
    %% 2012/01/18 退院 #
    %% 2012/02/05 外来
    HL7List =
        lists:map(fun({Date, MsgType}) ->
                          build_hl7msg(PatientID, Date, MsgType)
                  end,
                  [{<<"20111220">>, other},
                   {<<"20120110">>, admission},
                   {<<"20120115">>, discharge},
                   {<<"20120116">>, admission},
                   {<<"20120118">>, discharge},
                   {<<"20120118">>, admission},
                   {<<"20120118">>, discharge},
                   {<<"20120205">>, other}]),

    HitDates = [ Date || #hl7msg{date=Date} <- HL7List ],
    Endpoints = episode_extractor:get_endpoints(HL7List),
    ?assert(length(Endpoints) > 0),

    %% この時、
    %% エピソード1: 1900/01/01 - 2012/01/10 -a
    %% エピソード2: 2012/01/10 - 2012/01/15 a-d
    %% エピソード3: 2012/01/15 - 2012/01/16 d-a
    %% エピソード4: 2012/01/16 - 2012/01/18 a-d
    %% エピソード5: 2012/01/18 - 2012/01/18 d-a
    %% エピソード6: 2012/01/18 - 2012/01/18 a-d
    %% エピソード7: 2012/01/18 - 3000/12/31 a-
    ?assertEqual([{<<"19000101">>,<<"20120110">>},
                  {<<"20120110">>,<<"20120115">>},
                  {<<"20120115">>,<<"20120116">>},
                  {<<"20120116">>,<<"20120118">>},
                  {<<"20120118">>,<<"20120118">>},
                  {<<"20120118">>,<<"20120118">>},
                  {<<"20120118">>,<<"30001231">>}], Endpoints),

    Episodes0 = episode_extractor:process_episodes(
                 PatientID, HitDates, HL7List, [], ""),
    ?assert(length(Episodes0) > 0),

    Episodes = [Episode || {_, Episode, _} <- Episodes0],

    StartsEnds = [ {Start, End} ||
                     #episode{start_date = Start,
                              end_date = End} <- Episodes ],

    ?assertEqual([{<<"19000101">>,<<"20120110">>},
                  {<<"20120110">>,<<"20120115">>},
                  {<<"20120115">>,<<"20120116">>},
                  {<<"20120116">>,<<"20120118">>},
                  {<<"20120118">>,<<"20120118">>},
                  {<<"20120118">>,<<"20120118">>},
                  {<<"20120118">>,<<"30001231">>}], StartsEnds),

    %% episodes are border inclusive
    [ ?assert(length(E) >= 2) || #episode{hl7_msgs=E} <- Episodes ].

-endif.
