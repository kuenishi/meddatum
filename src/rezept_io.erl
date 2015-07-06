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

-include_lib("riakc/include/riakc.hrl").
-include("rezept.hrl").
-include("meddatum.hrl").

-export([delete_from_file/2]).

delete_from_file(C, Filename) ->
    KeyPrefix = rezept:key_prefix(Filename),
    Start = KeyPrefix,
    End   = <<KeyPrefix/binary, 255>>,
    Bucket = rezept:bucket(boom),
    case riakc_pb_socket:get_index_range(C, Bucket, <<"$key">>, Start, End) of
        {ok, #index_results_v1{keys=Keys}} ->
            {ok, lists:foldl(fun(ok, {OK,E}) -> {OK +1, E};
                                (_,  {OK,E}) -> {OK, E+1 } end, {0, 0},
                             [riakc_pb_socket:delete(C, Bucket, Key) || Key <- Keys])};
        E -> E
    end.
