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

-module(md_record).

-type md_record() :: tuple(). %% record data type

-include("meddatum.hrl").
-export([bucket2hospital_id/1]).

%% note that the file is usually *NOT* JSON.
-callback from_file(filename:filename(), list(), pid()) -> {ok, [md_record()]}.
-callback from_file(filename:filename(), list(), pid(), PostProcessor::fun()) -> {ok, [md_record()]}.

-callback to_json(md_record()) ->  {ok, JSON::binary()}.
-callback from_json(JSON::binary()) -> md_record().
-callback key(md_record()) -> binary().
-callback bucket(md_record()) -> binary().
-callback patient_id(md_record()) -> binary().
-callback hospital_id(md_record()) -> binary().

bucket2hospital_id({_, Bucket}) -> bucket2hospital_id(Bucket);
bucket2hospital_id(Bucket) when is_binary(Bucket) ->
    case Bucket of
        <<"ssmix:", HospitalID/binary>> ->
            {ssmix, HospitalID};
        <<"ssmix-patients:", HospitalID/binary>> ->
            {ssmix_patients, HospitalID};
        <<"recept:", HospitalID/binary>> ->
            {recept, HospitalID}
    end.
