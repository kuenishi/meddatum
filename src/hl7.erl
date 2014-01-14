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

-module(hl7).

-export([parse/2, to_json/1, from_json/1, annotate/1,
         get_segment/2,
         msg_type/1]).

-include_lib("eunit/include/eunit.hrl").
-include("hl7.hrl").

-spec annotate(#hl7msg{}) -> #hl7msg{}.
annotate(HL70 = #hl7msg{segments=Segs, hospital_id=HospitalID0}) ->
    {PatientID, HospitalID} = extract(Segs, {undefined, HospitalID0}),
    HL70#hl7msg{patient_id=PatientID, hospital_id=HospitalID}.

extract([], Tuple) -> Tuple;
extract([Seg|Segs], {P0, H0}) ->
    case Seg of
        [{<<"segid">>, <<"PID">>}|Rest] ->
            case proplists:get_value(<<"idlist">>, Rest) of
                undefined ->
                    extract(Segs, {P0, H0});
                IDList ->
                    ID = proplists:get_value(id, IDList),
                    {ID, H0}
            end;
        _ ->
            extract(Segs, {P0, H0})
    end.

get_segment(#hl7msg{segments = Segments}, SegName) ->
    lists:foldl(fun({Segment}, Acc0) ->
                        case proplists:get_value(<<"segid">>, Segment) of
                            SegName -> {ok, Segment};
                            _ -> Acc0
                        end
                end, {error, not_found}, Segments).

msg_type(#hl7msg{msg_type_s=MsgType}) when is_binary(MsgType) ->
    binary_to_list(MsgType);
msg_type(#hl7msg{msg_type_s=MsgType}) -> MsgType.

-spec parse(filename:filename(), file:file_info()) -> ok | {error, any()}.
parse(Filename, Info)->
    hl7_parser:parse(Filename, Info).

decoder() ->
    jsonx:decoder([{hl7msg, record_info(fields, hl7msg)}],
                  [{ignore, [null]}]).

encoder() ->
    jsonx:encoder([{hl7msg, record_info(fields, hl7msg)}],
                  [{ignore, [null]}]).

from_json(Json) when is_binary(Json) ->
    D = decoder(),
    D(Json).

to_json(#hl7msg{segments=_Segs} = HL7Msg) ->
    E = encoder(),
    case E(HL7Msg) of
        Bin when is_binary(Bin) -> Bin;
        Other -> error(Other)
    end.
