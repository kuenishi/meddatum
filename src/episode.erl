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

-module(episode).

-include_lib("meddatum/include/rezept.hrl").
-include_lib("meddatum/include/hl7.hrl").
-include_lib("meddatum/include/meddatum.hrl").
-include_lib("meddatum/include/episode.hrl").

-export([new/4, save/5,
         save_records/4]).

-export([extract_date/1]).


-spec new(binary(), binary(), list(), string()) -> #episode{}.
new(Admission, Discharge, Messages, Margin) ->

    Start = margin(Admission, Margin, sub),
    End   = margin(Discharge, Margin, add),
    %% ?debugVal({Start,End}),
    #episode{
       start_date = Start,
       end_date   = End,
       margin     = Margin,
       admission  = Admission,
       discharge  = Discharge,
       hl7_msgs   = extract_episode_(Start, End, Messages, [])
      }.

%% @private
%% <<"20130101HHMM">> + "1y" "1m" "1w" "1d" => <<"2013mmddHHMM">>
-spec margin(binary(), string(), add|sub) -> binary().
margin(Date, "", _) -> Date;
margin(Date, Margin, Op) when is_binary(Date) ->
    [Y0,Y1,Y2,Y3,M0,M1,D0,D1] = string:substr(binary_to_list(Date), 1, 8),
    Rest = string:substr(binary_to_list(Date), 9),
    Ddiff = case string:to_integer(Margin) of
                {Wdiff, "w"} -> Wdiff * 7;
                {Ddiff0, "d"} -> Ddiff0
            end,

    DateTime = {list_to_integer([Y0,Y1,Y2,Y3]),
                list_to_integer([M0,M1]),
                list_to_integer([D0,D1])},
    NewDate = case Op of
                  add -> calendar:date_to_gregorian_days(DateTime) + Ddiff;
                  sub -> calendar:date_to_gregorian_days(DateTime) - Ddiff
              end,
    {Year, Month, Day} = calendar:gregorian_days_to_date(NewDate),
    list_to_binary(integer_to_list(Year) ++
                       string:right(integer_to_list(Month), 2, $0) ++
                       string:right(integer_to_list(Day), 2, $0) ++ Rest).


%% @private
-spec extract_date(#hl7msg{}) -> binary().
extract_date(#hl7msg{date=Date0} = _) -> Date0;
extract_date(#recept{date=Date1} = _) -> Date1.

extract_and_trim_date(HL7orRecept) ->
    <<Bin:8/binary, _/binary>> = extract_date(HL7orRecept),
    Bin.

%% @private hl7 message from ssmix bucket
extract_episode_(_, _, [], List) -> lists:reverse(List);
extract_episode_(Start, End, [HL7orRecept|Rest], List)->
    case extract_and_trim_date(HL7orRecept) of
        Date when Date < Start-> %% left
            extract_episode_(Start, End, Rest, List);
        Date when End < Date -> %% right
            lists:reverse(List);
        _Date -> %% in time range, border inclusive
            extract_episode_(Start, End, Rest, [HL7orRecept|List])
    end.



%% @public
save_records(PatientID, HospitalID, StaticRecords, Dir) ->
    DirPath = filename:join([Dir,
                             binary_to_list(HospitalID),
                             binary_to_list(PatientID),
                             "0"]),
    ok = filelib:ensure_dir(DirPath ++ "/foobar"),
    lists:foreach(fun(HL7Msg)->
                          save_hl7(HL7Msg, DirPath, PatientID)
                  end,
                  StaticRecords).

%% @private
save(PatientID, Num,
     #episode{hl7_msgs = HL7Msgs, margin=Margin,
              start_date=Start0, end_date=End0},
     Recepts, Dir) ->
    Start = margin(Start0, Margin, add),
    End = margin(End0, Margin, sub),
    HospitalID = get_hospital_id(HL7Msgs, Recepts),
    EpisodeDir = io_lib:format("~p_~s_~s_~s",
                               [Num, Start, End, Margin]),
    DirPath = filename:join([Dir,
                             binary_to_list(HospitalID),
                             binary_to_list(PatientID),
                             EpisodeDir]),

    ok = filelib:ensure_dir(filename:join([DirPath, "foobar"])),
    lists:foreach(fun(HL7Msg)-> save_hl7(HL7Msg, DirPath, PatientID) end, HL7Msgs),
    lists:foreach(fun(Recept) -> save_recept(Recept, DirPath, PatientID) end, Recepts),
    %% io:format("[info] ~p ssmix and ~p recept saved to ~s", [length(HL7Msgs), length(Recepts),
    %%                                                  DirPath]),

    _ = lager:info("~p ssmix and ~p recept saved to ~s",
                   [length(HL7Msgs), length(Recepts), DirPath]).

%% @private
get_hospital_id([#hl7msg{hospital_id=HospitalID} = _HL7Msg|_],
                _) when is_binary(HospitalID) -> HospitalID;
get_hospital_id([], [#recept{hospital_id=HospitalID} = _Recept|_]
               ) when is_binary(HospitalID) -> HospitalID.

%% @private
-spec save_hl7(#hl7msg{}, filename:filename(), binary()) -> no_return().
save_hl7(#hl7msg{file=Path} = HL7Msg, DirPath, PatientID) ->
    Filename0 = lists:last(filename:split(binary_to_list(Path))),
    Filename = filename:join([DirPath, Filename0]) ++ ".json",

    ok = file:write_file(Filename, element(2, hl7:to_json(HL7Msg))),
    _ = lager:debug("~p saved to ~s~n", [PatientID, Filename]).
    %% io:format("[debug] ~p saved to ~s~n", [PatientID, Filename]).

%% @private
-spec save_recept(rezept:recept(), filanem:filename(), binary()) -> no_return().
save_recept(Recept, DirPath, PatientID) ->
    Filename0 = binary_to_list(extract_date(Recept)) ++ "_recept_",
    MD5 = binary_to_list(checksum:bin_to_hexbin(erlang:md5(term_to_binary(Recept)))),
    Filename = filename:join([DirPath, Filename0++MD5]) ++ ".json",

    case rezept:to_json(Recept) of
        {ok, JSON} when is_binary(JSON) ->
            ok = file:write_file(Filename, JSON),
            _ = lager:debug("~p saved to ~s~n", [PatientID, Filename]);
        Error ->
            io:format("[error] ~p ~p~n", [PatientID, Error])
    end.

-ifdef(TEST).

filename_test()->
    save_recept(#recept{date = <<"dummydate">>}, "/tmp", <<"foobar">>).

-endif.
