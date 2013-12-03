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

-export([walk/1, walk/3, walk2/3]).
-include_lib("eunit/include/eunit.hrl").
-include_lib("meddatum/include/hl7.hrl").

walk(Path) ->
    walk(Path, localhost, 8087).

walk2(Path, Host, Port) ->
    {ok,C}=ssmix_importer:connect(Host, Port),
    F = fun(File, Acc0) ->
                case hl7:parse(File, undefined) of
                    {ok, HL7Msg0} ->
                        HL7Msg = hl7:annotate(HL7Msg0#hl7msg{file=list_to_binary(File)}),
                        try
                            ok=ssmix_importer:put_json(C, HL7Msg)
                        catch T:E ->
                                lager:error("~p:~p", [T,E])
                        end,
                        Acc0;
                    {error, _Reason} = R -> [_Reason|R]
                end
        end,
    _ErrorFiles = filelib:fold_files(Path, "", true, F, []),
    ok=ssmix_importer:disconnect(C).

walk(Path, Host, Port) ->
    {ok, Pid} = ssmix_walker:start_link(Path),
    {ok,C}=ssmix_importer:connect(Host, Port),
    _ = lager:info("connected.~p~n", [C]),
    timer:sleep(100),
    wait_for_walker(Pid, C, 0).

wait_for_walker(Pid, C, N) ->
    {Flag, HL7Msgs} = ssmix_walker:pop(Pid),

    lager:info("inserting ~p msgs..~n", [length(HL7Msgs)]),
    lists:foreach(fun(HL7Msg) ->
                          lager:info("inserting:~n", []),
			  try
                  ok=ssmix_importer:put_json(C, HL7Msg)
			  catch T:E ->
                      _ = lager:error("~p:~p", [T,E])
              end,
                          lager:info("done.~n", [])
                  end, HL7Msgs),
    N2 = length(HL7Msgs) + N,
    lager:info("~p msgs stored.~n", [length(HL7Msgs)]),
    case Flag of
        ok -> ok=ssmix_importer:disconnect(C);
        cont -> wait_for_walker(Pid, C, N2)
    end.
