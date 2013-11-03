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

-module(ssmix).

-export([walk/1]).
-include_lib("eunit/include/eunit.hrl").

walk(Path) ->
    {ok, Pid} = ssmix_walker:start_link(Path),
    {ok,C}=ssmix_importer:connect(localhost, 8087),
    timer:sleep(100),
    wait_for_walker(Pid, C, 0).

wait_for_walker(Pid, C, N) ->
    {Flag, HL7Msgs} = ssmix_walker:pop(Pid),
    %% ?debugVal(Flag),
    lists:foreach(fun(HL7Msg) ->
                          ok=ssmix_importer:put_json(C, HL7Msg)
                  end, HL7Msgs),
    N2 = length(HL7Msgs) + N,
    meddatum:log(info, "~p msgs stored.~n", [length(HL7Msgs)]),
    case Flag of
        ok -> ok=ssmix_importer:disconnect(C);
        cont -> wait_for_walker(Pid, C, N2)
    end.
