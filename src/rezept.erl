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

-behaviour(md_record).

-include_lib("eunit/include/eunit.hrl").
-include("rezept.hrl").
-include("meddatum.hrl").
-include("md_json.hrl").

-export([from_json/1, to_json/1,
         key/1, key_prefix/1,
         bucket/1, bucket_from_hospital_id/1,
         make_2i_list/1,
         patient_id/1, hospital_id/1,
         columns/0,
         from_file/3, from_file/4
         ]).

-export([
         append_to_recept/2,
         finalize/1, subtables/0
        ]).

-type recept() :: #recept{}.
-export_type([recept/0]).

-spec encoder() -> {error, any(), any()}|{no_match, term()}|binary().
encoder() ->
    ?JSON_RECORD_ENCODER(recept).
    %% md_json:encoder([{recept, record_info(fields, recept)}],
    %%                 [{ignore, [null]}]).

decoder() ->
    ?JSON_RECORD_DECODER(recept).
    %% md_json:decoder([{recept, record_info(fields, recept)}],
    %%                 [{ignore, [null]}]).

-define(ENCODER, (encoder())).
-define(DECODER, (decoder())).

-spec from_file(filename:filename(), [med|dpc], pid()) -> {ok, [#recept{}]}.
from_file(Filename, [Mode], Logger) when Mode =:= med orelse Mode =:= dpc ->
    rezept_parser:parse_file(Filename, Mode, Logger).

from_file(Filename, [Mode], Logger, PostProcessor) when Mode =:= med orelse Mode =:= dpc ->
    rezept_parser:parse_file(Filename, Mode, Logger, PostProcessor).

-spec to_json(#recept{}) -> {ok, binary()} | {error, term()}.
to_json(Rezept) when is_record(Rezept, recept) > 0 ->

    case ?ENCODER(Rezept) of
        {error, A, B} ->
            %% _ = lager:error("to_json: ~p, ~p~n", [A, B]),
            {error, {A,B}};
        {no_match, _O} ->
            %% _ = lager:error("~p~n", [Rezept]),
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

-spec bucket(#recept{}) -> binary().
bucket(#recept{hospital_id = HospitalID} = _Recept) ->
    bucket_from_hospital_id(HospitalID).

-spec bucket_from_hospital_id(binary()) -> {binary(), binary()}.
bucket_from_hospital_id(HospitalID) when is_binary(HospitalID) ->
    BucketName = <<?RECEPT_BUCKET/binary, ?BUCKET_NAME_SEPARATOR/binary, HospitalID/binary>>,
    {?BUCKET_TYPE, BucketName}.

make_2i_list(#recept{patient_id=PatientID, date=Date}) ->
    [{"date", Date}, {"patient_id", PatientID}].

append_to_recept(#recept{segments=List} = Recept, Data) ->
    Recept#recept{segments=[Data|List]}.

has_re(#recept{segments=List} = _Recept) ->
    HasRE = fun(Segment) ->
                    case proplists:get_value(record_info, Segment) of
                        <<"RE">> -> true;
                        _ -> false
                    end
            end,
    lists:any(HasRE, List).

finalize(#recept{segments=List, file=_File} = Recept) ->
    case has_re(Recept) of
        true -> ok;
        false -> pass
            %% _ = lager:warning("~p doesn't have RE record", [File])
    end,

    %% compensate the omitted IY/SI/TOs
    List1 = compensate(undefined, lists:reverse(List), []),

    NewList = [{rezept_parser:postprocess(Seg, Recept)}|| Seg <- List1],
    %% Length = length(List) = length(List1) = length(NewList),
    Recept#recept{segments=NewList}.

compensate(undefined, [H|L], Acc) -> compensate(H, L, [H|Acc]);
compensate(_, [], Acc) ->            lists:reverse(Acc);
compensate(PrevSeg, [Seg|L], Acc) ->
    %% SI -> shin_identifier
    %% IY -> shin_identifier
    %% TO -> shin_identifier
    TryCompensate =
        case proplists:get_value(record_info, Seg) of
            <<"SI">> -> true;
            <<"IY">> -> true;
            <<"TO">> -> true;
            _ -> false
        end,
    case TryCompensate of
        true ->
            NewSeg = compensate_shin_identifier(PrevSeg, Seg),
            compensate(NewSeg, L, [NewSeg|Acc]);
        false ->
            compensate(Seg, L, [Seg|Acc])
    end.

compensate_shin_identifier(PrevSeg, Seg) when is_list(PrevSeg) ->
    case proplists:get_value(shin_identifier, Seg) of
        I when is_integer(I) -> Seg;
        _ ->
            case proplists:get_value(shin_identifier, PrevSeg) of
                J when is_integer(J) ->
                    replace_prop(shin_identifier, J, Seg);
                _ -> Seg
            end
    end.

replace_prop(Key, Value, Proplist) ->
    replace_prop(Key, Value, Proplist, []).

replace_prop(Key, Value, [], Acc) ->
    lists:reverse([{Key,Value}|Acc]);
replace_prop(Key, Value, [{Key,_}|TL], Acc) ->
    rev_append(Acc, [{Key,Value}|TL]);
replace_prop(Key, Value, [HD|TL], Acc) ->
    replace_prop(Key, Value, TL, [HD|Acc]).


rev_append([], Right) -> Right;
rev_append([HD|TL], Right) -> rev_append(TL, [HD|Right]).


patient_id(#recept{patient_id=PatientID}) -> PatientID.
hospital_id(#recept{hospital_id=HospitalID}) -> HospitalID.


columns() ->
    [
     [{name, date},        {type, 'varchar'}, {index, true}],
     [{name, patient_id},  {type, 'varchar'}, {index, true}],
     [{name, hospital_id}, {type, 'varchar'}, {index, false}],
     [{name, file},        {type, 'varchar'}, {index, false}],
     [{name, checksum},    {type, 'varchar'}, {index, false}]
    ].

subtables() ->
    %% ?MED_RECORD_TYPES and ?DPC_RECORD_TYPES both have part, which
    %% is ?REZEPT_COMMON_RECORDS. So duplication is removed by sets
    RecordTypes = sets:union(sets:from_list(?DPC_RECORD_TYPES),
                             sets:from_list(?MED_RECORD_TYPES)),
    [{<<"subtables">>,
      lists:map(fun(C) -> recept_record_to_presto_nested_column(C) end,
                sets:to_list(RecordTypes))}].

recept_record_to_presto_nested_column({Rinfo0, _, RecordTypes0}) ->
    SubtableColumns =
        lists:map(fun({Name0, Type, _Length}) ->
                          {[{name, Name0},
                            {type, type_recept2presto(Type)},
                            {index, false}]}
                  end,
                  handle_30days_schema(RecordTypes0)),
    Rinfo = list_to_binary(Rinfo0),
    {[{name, Rinfo},
      {path, <<"$.segments[?(@.record_info=='", Rinfo/binary, "')]">>},
      {columns, SubtableColumns}
     ]}.

%% @doc see rezept_parser:check_type/2
type_recept2presto({maybe, Type}) -> type_recept2presto(Type);
type_recept2presto(integer) ->  bigint;
type_recept2presto(latin1) -> varchar;
type_recept2presto(unicode) -> varchar;
type_recept2presto(date) -> varchar;
type_recept2presto(gyymm) -> varchar;
type_recept2presto(gyymmdd) -> varchar;
type_recept2presto(jy_code) -> varchar;
type_recept2presto(prefecture) -> varchar.

%% @doc see rezept_parser:handle_30days/2
handle_30days_schema(RecordTypes0) ->
    RecordTypes =
        lists:filter(
          fun(info_1) -> false;
             (info_2) -> false;
             (info_3) -> false;
             (info_4) -> false;
             (info_5) -> false;
             (info_6) -> false;
             (info_7) -> false;
             (info_8) -> false;
             (info_9) -> false;
             (info_10) -> false;
             (info_11) -> false;
             (info_12) -> false;
             (info_13) -> false;
             (info_14) -> false;
             (info_15) -> false;
             (info_16) -> false;
             (info_17) -> false;
             (info_18) -> false;
             (info_19) -> false;
             (info_20) -> false;
             (info_21) -> false;
             (info_22) -> false;
             (info_23) -> false;
             (info_24) -> false;
             (info_25) -> false;
             (info_26) -> false;
             (info_27) -> false;
             (info_28) -> false;
             (info_29) -> false;
             (info_30) -> false;
             (info_31) -> false;
             (_) -> true
          end,
          RecordTypes0),

    %% TODO: how can we do search this with?
    [{'history-date', gyymmdd, 0},
     {'history-cnt', integer, 0}] ++ RecordTypes.

%% {
%%     name: "ho", => shown as 'tablename:ho' in Presto
%%     path: "$.segments[?(@.record_info=='HO')]",
%%     columns: [
%%         {name: "l", type: "varchar", index: true},
%%         {name: "record_info", type: "varchar", index: true},
%%         {name: "hokno", type: "varchar", index: true},
%%         {name: "kigo", type: "varchar", index: true},
%%         {name: "bango", type: "varchar", index: true},
%%         {name: "hoknissu", type: "bigint", index: true},
%%         {name: "hokten", type: "bigint", index: true},
%%     ]
%% }
