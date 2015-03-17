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

-module(rezept_io).

-include_lib("eunit/include/eunit.hrl").
-include_lib("riakc/include/riakc.hrl").
-include("rezept.hrl").
-include("meddatum.hrl").

-export([put_record/2, delete_from_file/2]).

put_record(C, Record0) ->

    case rezept:to_json(Record0) of
        {ok, JSONRecords} ->
            %%ok = file:write_file("test.json", JSONRecords).
            ContentType = "application/json",
            %% Key = list_to_binary(integer_to_list(erlang:phash2(JSONRecords))),
            Key = rezept:key(Record0),
            Bucket = rezept:bucket(Record0),
            RiakObj0 = meddatum:maybe_new_ro(C, Bucket, Key, JSONRecords, ContentType),

            %% put indices to all member
            RiakObj = set_2i(RiakObj0, Record0),
            riakc_pb_socket:put(C, RiakObj);
        {error, empty} ->
            error({empty_record, Record0});
        Other ->
            %% TODO: insert probe code to find bad data format or spec.
            %% lists:foreach(fun(R) ->
            %%                       lists:foreach(fun(P) ->
            %%                                             io:format("~n~p~n", [[P]]),
            %%                                             {ok, _} = to_json([P])
            %%                                     end, R)
            %%               end, proplists:get_value(<<"segments">>, Record0)),
            error(Other)
    end.

set_2i(RiakObj0, Record0) ->
    RiakObj1 = case Record0#recept.patient_id of
                   undefined -> RiakObj0;
                   PatientID ->
                       MD0 = riakc_obj:get_update_metadata(RiakObj0),
                       MD1 = riakc_obj:set_secondary_index(MD0, {{binary_index, ssmix_importer:index_name(patient_id)}, [PatientID]}),
                       riakc_obj:update_metadata(RiakObj0, MD1)
               end,
    Date = Record0#recept.date,
    MD2 = riakc_obj:get_update_metadata(RiakObj1),
    MD3 = riakc_obj:set_secondary_index(MD2, {{binary_index, ssmix_importer:index_name(date)}, [Date]}),
    riakc_obj:update_metadata(RiakObj1, MD3).

delete_from_file(C, Filename) ->
    KeyPrefix = rezept:key_prefix(Filename),
    Start = KeyPrefix,
    End   = <<KeyPrefix/binary, 255>>,
    Bucket = meddatum:true_bucket_name(?RECEPT_BUCKET),
    case riakc_pb_socket:get_index_range(C, Bucket, <<"$key">>, Start, End) of
        {ok, #index_results_v1{keys=Keys}} ->
            {ok, lists:foldl(fun(ok, {OK,E}) -> {OK +1, E};
                                (_,  {OK,E}) -> {OK, E+1 } end, {0, 0},
                             [riakc_pb_socket:delete(C, Bucket, Key) || Key <- Keys])};
        E -> E
    end.
