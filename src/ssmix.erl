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

-export([walk2/4]).
-include_lib("eunit/include/eunit.hrl").
-include_lib("meddatum/include/hl7.hrl").

walk2(Path, HospitalID, Host, Port) ->
    {ok,C}=ssmix_importer:connect(Host, Port),
    F = fun(File, Acc0) ->
                _ = lager:info("Processing ~p ~p", [File, HospitalID]),
                case hl7:parse(File, undefined) of
                    {ok, HL7Msg0} ->
                        HL7Msg = hl7:annotate(HL7Msg0#hl7msg{file=list_to_binary(File),
                                                             hospital_id=HospitalID}),
                        try
                            ok=ssmix_importer:put_json(C, HL7Msg)
                        catch T:E ->
                                lager:error("~p:~p", [T,E])
                        end,
                        Acc0;
                    {error, _Reason} = R ->
                        _ = lager:error("~p:~p", [File, R]),
                        [_Reason|R]
                end
        end,
    _ErrorFiles = filelib:fold_files(Path, "", true, F, []),
    ok=ssmix_importer:disconnect(C).
