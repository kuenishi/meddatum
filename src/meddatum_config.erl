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

-module(meddatum_config).

-export([use_bucket_types/0,
         check_riak_connectivity/0,
         get_riak/0,
         get_config/0]).

-spec use_bucket_types() -> boolean().
use_bucket_types() -> true.

-spec check_riak_connectivity() -> boolean().
check_riak_connectivity() ->
    {ok, {Host, Port}} = get_riak(),
    {ok, Pid} = riakc_pb_socket:start_link(Host, Port),
    {ok, _Indexes} = riakc_pb_socket:list_search_indexes(Pid),
    ok = riakc_pb_socket:stop(Pid),
    true.

get_riak() ->
    case get_config() of
        {ok, Config} ->
            Host = proplists:get_value(riak_ip, Config),
            Port = proplists:get_value(riak_port, Config),
            {ok, {Host, Port}};
        {error, enoent} = E ->
            io:format("~~/.meddatum is required to run meddatum.~n"
                      "run 'meddatum create-config' to create first template~n"
                      "and then ocnfigure it.~n"),
            E;
        {error, _} = E ->
            io:format("Error: ~p", [E]),
            E
    end.


get_config() ->
    Filename = os:getenv("HOME")++"/.meddatum",
    file:consult(Filename).
