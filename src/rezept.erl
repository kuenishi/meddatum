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
         bucket/1,
         patient_id/1, hospital_id/1,
         columns/0,
         from_file/3, from_file/4
         ]).

-export([
         append_to_recept/2,
         finalize/1
        ]).

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

-spec to_json(#recept{}) -> {ok, binary()}.
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
    BucketName = <<?RECEPT_BUCKET/binary, ?BUCKET_NAME_SEPARATOR/binary, HospitalID/binary>>,
    case meddatum_config:use_bucket_types() of
        true ->
            {?BUCKET_TYPE, BucketName};
        false ->
            ?RECEPT_BUCKET
    end.

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
     [{name, date},        {type, 'STRING'}, {index, true}],
     [{name, patient_id},  {type, 'STRING'}, {index, true}],
     [{name, hospital_id}, {type, 'STRING'}, {index, true}],
     %%[{name, segments},    {type, 'STRING'}, {index, true}],
     [{name, file},        {type, 'STRING'}, {index, true}],
     [{name, checksum},    {type, 'STRING'}, {index, true}]
    ].
