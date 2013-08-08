%%% @copyright (C) 2013, Basho Technologies Inc.,
%%% @doc
%%%  parse electric rezept and put it into Riak
%%% @end
%%% Created : 02 Aug 2013 by UENISHI Kota <kota@basho.com>

-module(rezept).

-include_lib("eunit/include/eunit.hrl").
-include("rezept.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([process_file/2, put_record/1]).

-record(rezept, {
         }).

-spec process_file(filename:filename(), file:file_info()) -> ok.
process_file(Filename, _Info) ->
    {ok, Lines} = japanese:read_file(Filename),
    %% io:format("~p~n", [Lines]),
    F = fun({newline, NewLine}, Ctx0) ->
                {ok, Ctx} = parse_line(NewLine, Ctx0),
                Ctx;
           (_, Ctx0) -> Ctx0
        end,
    {ok, ResultCtx} = ecsv:process_csv_string_with(lists:flatten(Lines), F, {[], []}),
    {Remain, Records0} = ResultCtx,
    Records = [ lists:reverse(Remain) | Records0 ],
    io:format("~p records.~n", [length(Records)]),

    %%lists:foreach(fun put_record/1, Records),
    %%put_record(hd(Records)),
    ok.

put_record(Record0) ->
    Fun = fun({K,V}) when is_list(V) -> 
                  {unicode:characters_to_binary(K),
                   unicode:characters_to_binary(V)};
             ({K,V})->
                  {unicode:characters_to_binary(K), V}
          end,
    Record = [[Fun(Elem) || Elem <- Line] || Line <- Record0 ],
    JSONRecords= jsonx:encode(Record),
    io:format("~n~n"),
    io:format("~ts~n", [JSONRecords]),
    %%io:put_chars(JSONRecords),
    ok = file:write_file("test.json", JSONRecords).

parse_line(Line, {List, Records}) ->
    [_DataID, _, _, RecordID|_] = Line,
    %% io:format("~p~n", [Line]),
    case lists:keytake(RecordID, 1, ?RECORD_TYPES) of
        {value, {RecordID, Name, Cols0}, _} ->
            io:format("[~s]~s~n", [RecordID, Name]),
            ShortLen = erlang:min(length(Line), length(Cols0)),
            {Line1,_} = lists:split(ShortLen, Line),
            {Cols, _} = lists:split(ShortLen, Cols0),
            Data0 = lists:map(fun({Col, Entry}) ->
                                     %% ?debugVal(Entry),
                                     case check_type(Col, Entry) of
                                         {ok, Ret} -> Ret;
                                         {warning, Ret} ->
                                             io:format("[warning]: ~s~n", [RecordID]),
                                             Ret
                                     end
                             end,
                             lists:zip(Cols, Line1)),
            Data = lists:filter(fun({_,null}) -> false; (_) -> true end,
                                Data0),
            %% ?debugVal(Data),
            case RecordID of

                "MN" ->
                    {ok, {[Data], [lists:reverse(List)|Records]}};
                _ ->
                    NewList = [Data|List],
                    {ok, {NewList, Records}}
            end;
        {value, _, _} ->
            {error, {not_yet, RecordID}};
        false ->
            {error, {unknown_record, RecordID}}
    end.

check_type({Name, {maybe, _}, _}, []) -> {ok, {Name, null}};
check_type({Name, {maybe, Type}, MaxDigits}, Entry) ->
    check_type({Name, Type, MaxDigits}, Entry);
check_type({Name, _, _}, []) ->
    io:format("[warning]: empty value which is not optional: ~s~n", [Name]),
    {warning, {Name, null}};
check_type({Name, integer, _MaxDigits}, Entry) ->
    {ok, {Name, list_to_integer(Entry)}};
check_type({Name, latin1, _MaxDigits}, Entry) ->  {ok, {Name, Entry}};
check_type({Name, unicode, _MaxDigits}, Entry) -> 
    %% ?debugVal(Entry),
    %% io:format(Entry),
    %% io:format(">>>> => ~ts~n", [unicode:characters_to_binary(Entry, utf8)]),
    {ok, {Name, Entry}};
check_type({Name, gyymm, 5}, Entry) ->  {ok, {Name, Entry}};
check_type({Name, gyymmdd, 7}, Entry) ->  {ok, {Name, Entry}};
check_type({Name, jy_code, _}, Entry) when length(Entry) =:= 1 ->
    {ok, {Name, Entry}}.



%% rid2fun("MN") -> 'MN';
%% rid2fun(Other) -> error({undefined,Other}).

