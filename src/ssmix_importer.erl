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

-module(ssmix_importer).
-export([connect/2, disconnect/1, put_json/2,
         bucket_name/1,
         index_name/1]).

-include("hl7.hrl").
-include("meddatum.hrl").
-include_lib("eunit/include/eunit.hrl").

-spec connect(term(), inet:port_number()) -> {ok, pid()}.
connect(Host, Port) ->
    riakc_pb_socket:start_link(Host, Port).

disconnect(Client) ->
    riakc_pb_socket:stop(Client).

bucket_name(#hl7msg{msg_type_s=MsgType}) ->
    IsStaticRecord =
        case MsgType of
            <<"ADT^A08", _/binary>> -> true; %% 患者基本情報の更新
            <<"ADT^A23", _/binary>> -> true; %% 患者基本情報の削除
            <<"ADT^A54", _/binary>> -> true; %% 担当医の変更
            <<"ADT^A55", _/binary>> -> true; %% 担当医の取消
            <<"ADT^A60", _/binary>> -> true; %% アレルギー情報の登録／更新
            <<"PPR^ZD1", _/binary>> -> true; %% 病名(歴)情報の登録／更新
            _B when is_binary(_B) -> false
        end,
    BucketName = case IsStaticRecord of
                     true -> ?SSMIX_PATIENTS_BUCKET;
                     false -> ?SSMIX_BUCKET
                 end,
    meddatum:true_bucket_name(BucketName).

put_json(Client, Msg) ->
    %% TODO: Bucket, Key is to be extracted from msg
    ContentType = <<"application/json">>,
    Key = filename:basename(Msg#hl7msg.file),
    Data = hl7:to_json(Msg),
    Bucket = bucket_name(Msg),
    RiakObj0 = meddatum:maybe_new_ro(Client, Bucket, Key, Data, ContentType),

    _ = lager:debug("inserting: ~p~n", [Key]),
    RiakObj = set_2i(RiakObj0, Msg#hl7msg.date, Msg#hl7msg.patient_id),
    case riakc_pb_socket:put(Client, RiakObj) of
      ok -> ok;
      Error -> _ = lager:error("error inserting ~p: ~p", [Key, Error])
    end.

set_2i(RiakObj0, Date, PatientID) ->
    MD0 = riakc_obj:get_update_metadata(RiakObj0),
    MD1 = riakc_obj:set_secondary_index(MD0, {{binary_index, index_name(date)}, [Date]}),
    MD2 = riakc_obj:set_secondary_index(MD1, {{binary_index, index_name(patient_id)}, [PatientID]}),
    riakc_obj:update_metadata(RiakObj0, MD2).

index_name(patient_id) ->
    "patient_id";
index_name(date) ->
    "date".


-ifdef(TEST).

-define(assertBucketName(Exp, Val),
        ?assertEqual(Exp, bucket_name(#hl7msg{msg_type_s= Val}))).

bucket_name_test() ->
    ?assertBucketName(?SSMIX_PATIENTS_BUCKET, <<"ADT^A60foorbaz">>),
    ?assertBucketName(?SSMIX_BUCKET, <<"ADT^A61foobarbaz">>).

-endif.
