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
         index_name/1]).
-include("hl7.hrl").
-include_lib("eunit/include/eunit.hrl").

-spec connect(term(), inet:port_number()) -> {ok, pid()}.
connect(Host, Port) ->
    riakc_pb_socket:start_link(Host, Port).

disconnect(Client) ->
    riakc_pb_socket:stop(Client).

put_json(Client, Msg) ->
    %% TODO: Bucket, Key is to be extracted from msg
    ContentType = <<"application/json">>,
    Key = filename:basename(Msg#hl7msg.file),
    Data = hl7:to_json(Msg),
    RiakObj0 = riakc_obj:new(<<"ssmix">>, Key, Data, ContentType),
                          lager:info("inserting:~n", []),
    RiakObj = set_2i(RiakObj0, Msg#hl7msg.date, Msg#hl7msg.patient_id),
                          lager:info("inserting:~n", []),
    case riakc_pb_socket:put(Client, RiakObj) of
      ok -> ok;
      Error -> meddatum:log(error, "error: ~p", [Error])
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

