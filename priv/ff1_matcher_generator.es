#!/usr/bin/env escript

-compile(export_all).

main([CSVFile]) ->
    {ok, Fd} = file:open(CSVFile, [read]),
    {ok, Lines0} = file:read_line(Fd),
    try
        io:format("%% generated by ~s at ~p~n", [?FILE, erlang:localtime()]),
        io:format("%% Source Filename: ~p~n", [CSVFile]),
        io:format("%% data description: {<PayloadType>, [{<property name>, <required>, <datatype>}|{}]}~n"),
        io:format("-define(~s, {\"~s\", [~n", ["DUMMY", "DUMMY"]),
        Lines = tl(tl(re:split(Lines0, "\r", [{return, list}]))),
        handle_line(Lines, [])
        %[io:format("~p~n", [Line])||Line<-Lines0]
    after
        file:close(Fd)
    end;
main([]) ->
    io:format(standard_error, "usage: escript ff1_matcher_generator.es <csvfile> > include/ff1_types.hrl~n", []).

handle_line([], Codes) -> output_codes(lists:reverse(Codes));
handle_line([Line|Lines], Acc) ->
    Tokens = re:split(Line, ",", [{return, list}]),
    [_No, Code, _PayloadType, _, _Name, _Required, _Type, _FieldName, _] = Tokens,
    maybe_print(Code),
    Acc1 = case Code of [] -> Acc;
               _ -> [Code|Acc] end,
    print_element(Tokens),
    handle_line(Lines, Acc1).

maybe_print("") -> ok;
maybe_print(Code) ->
    io:format("    {}]}).~n"),
    io:format("-define(~s, {\"~s\", [~n", [Code, Code]).

print_element([_No, _Code, _PayloadType, Idx, _Name, Required, Type, FieldName, _] = _Tokens) ->
    io:format("    {~s, ~s, ~s, ~s},~n", [FieldName, Idx, Required, Type]).


output_codes(Codes) ->
    io:format("    {}]}).~n"),
    io:format("-define(DPCS_CODES, [~n"),
    [io:format("?~s, ", [Code])||Code<-Codes],
    io:format("?DUMMY]).~n").
