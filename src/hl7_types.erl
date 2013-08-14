-module(hl7_types).
-export([parse/1]).

-include("hl7.hrl").
-include_lib("eunit/include/eunit.hrl").

%% type printer for priv/generate

parse(Lines) ->
    Hd = string:substr(lists:flatten(hd(Lines)), 1, 3),
    io:format("-define(HL7_~s,~n        [~n", [Hd]),
    parse_0(tl(Lines)).


parse_0([Line]) ->
    Data = parse_1(Line),
    io:format("         {\"~s\", ~p, ~p, \"~s\"} %% ~s ~s~n", Data),
    io:format("        ]).~n~n");

parse_0([Line|Lines]) ->
    Data = parse_1(Line),
    io:format("         {\"~s\", ~p, ~p, \"~s\"}, %% ~s ~s~n", Data),
    parse_0(Lines).

parse_1(Line) ->
    [Code, Name, MaxLen0, Property0, Desc, Type0, Desc2, Optional] = Line,
    %% Same type def as rezept
    MaxLen = list_to_integer(MaxLen0),
    Type = case Optional of
               "TRUE"  -> {maybe, list_to_atom(Type0)};
               "FALSE" -> list_to_atom(Type0)
           end,
    Property = case Property0 of
                   "" -> reform(Desc);
                   _ -> Property0
               end,
    %%?debugVal(Line),
    [Property, Type, MaxLen, Name, Code, Desc2].

reform(Str0) ->
    Str1 = string:to_lower(string:strip(Str0)),
    Str2 = lists:map(fun(I) when I < 255 -> I;
                        (_) -> $_ end, Str1),
    re:replace(Str2, "\s", "_", [global,{return,list}]).
