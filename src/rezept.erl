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

-include_lib("eunit/include/eunit.hrl").
-include("rezept.hrl").

-export([from_json/1, to_json/1,
         key/1, key_prefix/1,
         append_to_recept/2,
         finalize/1,
         patient_id/1
        ]).

encoder() ->
    jsonx:encoder([{recept, record_info(fields, recept)}],
                  [{ignore, [null]}]).

decoder() ->
    jsonx:decoder([{recept, record_info(fields, recept)}],
                  [{ignore, [null]}]).

-define(ENCODER, (encoder())).
-define(DECODER, (decoder())).

%% maybe_taken(0, List) -> List;
%% maybe_taken(_, []) -> [];
%% maybe_taken(N, List) ->
%%     maybe_taken(N-1, tl(List)).

-spec to_json(#recept{}) -> {ok, binary()}.
to_json(Rezept) when is_record(Rezept, recept) > 0 ->

    case ?ENCODER(Rezept) of
        {error, A, B} ->
            _ = lager:error("to_json: ~p, ~p~n", [A, B]),
            {error, {A,B}};
        {no_match, _O} ->
            _ = lager:error("~p~n", [Rezept]),
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

append_to_recept(#recept{segments=List} = Recept, Data) ->
    Recept#recept{segments=[Data|List]}.

%% has_re(#recept{segments=List} = _Recept) ->
%%     HasRE = fun(Segment) ->
%%                    case proplists:get_value(record_info, Segment) of
%%                        <<"RE">> -> true;
%%                        _ -> false
%%                    end
%%              end,
%%     lists:any(HasRE, List).


finalize(#recept{segments=List} = Recept) ->
    %% true = has_re(Recept),
    NewList = lists:reverse(List),
    Recept#recept{segments=NewList}.

patient_id(#recept{patient_id=PatientID}) -> PatientID.
