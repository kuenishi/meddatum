-module(ff1_matcher).

-include("ff1_types.hrl").
-export([to_list/3]).

to_list(Code, Tokens, LineNo) ->
    PayloadDef = proplists:get_value(Code, ?DPCS_CODES),
    Obj = to_object(PayloadDef, Tokens, LineNo, []),
    Obj2 = verify_fields(PayloadDef, Obj, LineNo),
    Obj3 = postprocess_payload(Code, Obj2),
    orddict:from_list(Obj3).

to_object([{}], _, _, Acc) -> lists:reverse(Acc);
to_object([{Property, Idx, Required, Type}|Defs], Tokens, LineNo, Acc0) ->
    Token = lists:nth(Idx, Tokens),
    case {Required, Token} of
        {2, ""} ->  error({required_field, LineNo, Property});
        _ -> ok
    end,
    Value = case {Token, Type} of
                {"", _} -> null;
                _ -> Token
            end,
    to_object(Defs, Tokens, LineNo, [{Property, Value}|Acc0]).

verify_fields([{}], Obj, _) -> Obj;
verify_fields([{_, _, 0, _}|Defs], Obj, LineNo) ->
    verify_fields(Defs, Obj, LineNo);
verify_fields([{_, _, 2, _}|Defs], Obj, LineNo) ->
    verify_fields(Defs, Obj, LineNo);
verify_fields([{Property, _, 1, _}|Defs], Obj, LineNo) ->
    case proplists:get_value(Property, Obj) of
        undefined ->
            io:format("~p~n", [Obj]),
            error({required_field, LineNo, Property});
        "" ->
            io:format("~p~n", [Obj]),
            error({required_field, LineNo, Property});
        _ -> verify_fields(Defs, Obj, LineNo)
    end.


%% Postprocess a line; not field

%% 　A006010　診断情報/主傷病  mainsick_(cd|nm)
%% 　A006020　診断情報/入院契機 oppsick
%% 　A006030　診断情報/医療資源 maxsick, disaddcd
%% 　A006031　診断情報/医療資源2 nextsick
%% 　A006040　診断情報/併存症 heisick
%% 　A006050　診断情報/続発症 hassick
%% 　A007010　手術情報 ope, ...


-spec postprocess_payload(string(), proplists:proplist()) -> proplists:proplist().
postprocess_payload("A006010", Obj) -> %%診断情報/主傷病  mainsick_(cd|nm)
    tie_up_sick(mainsick_cd, mainsick_nm, Obj, 1);
postprocess_payload("A006020", Obj) -> %%診断情報/入院契機 oppsick
    tie_up_sick(oppsick_cd, oppsick_nm, Obj, 2);
postprocess_payload("A006030", Obj) -> %%診断情報/医療資源 maxsick, disaddcd
    tie_up_sick(maxsick_cd, maxsick_nm, Obj, 3);
postprocess_payload("A006031", Obj) -> %%診断情報/医療資源2 nextsick
    tie_up_sick(nextsick_cd, nextsick_nm, Obj, 4);
postprocess_payload("A006040", Obj) -> %%診断情報/併存症 heisick
    tie_up_sick(heisick_cd, heisick_nm, Obj, 5);
postprocess_payload("A006050", Obj) -> %%診断情報/続発症 hassick
    tie_up_sick(hassick_cd, hassick_nm, Obj, 6);
postprocess_payload("A007010", Obj) -> %%手術情報 ope, ...
    tie_up_ope(Obj);
postprocess_payload(_, Obj) ->
    Obj.

%% Dirty!!!
tie_up_sick(A, B, Obj, SickType) ->
    case lists:keytake(A, 1, Obj) of
        {value, {A, null}, _} ->
            error({notfound, A});
        {value, {A, CD0}, Obj1} ->
            %% all values in sick are str type
            CD = unicode:characters_to_binary(CD0, utf8, utf8),
            case lists:keytake(B, 1, Obj1) of
                {value, {B, null}, _} ->
                    error({notfound, B});
                {value, {B, NM0}, Obj2} ->
                    NM = unicode:characters_to_binary(NM0, utf8, utf8),
                    Sicks = [{sick_cd, CD}, {sick_nm, NM}, {sick_type, SickType}],
                    case lists:keytake(disaddcd, 1, Obj2) of
                        {value, {disaddcd, null}, Obj3} ->
                            [{sick, [{Sicks}]}|Obj3];
                        {value, {disaddcd, D0}, Obj3} ->
                            D = unicode:characters_to_binary(D0, utf8, utf8),
                            [{sick, [{[{disaddcd, D}|Sicks]}]}|Obj3];
                        false ->
                            [{sick, [{Sicks}]}|Obj2]
                    end;
                false ->
                    %% can't happen this
                    error({notfound, B})
            end;
        false ->  %% Maybe happen
            error({notfound, A})
    end.

tie_up_ope(Obj) ->
    Keys = [{ope_opeymd, opeymd},
            {ope_tencd, tencd},
            {ope_cnt, cnt},
            {ope_side, side},
            {ope_ane, ane},
            {ope_nm, nm}],
    tie_up_ope(Keys, Obj, []).

tie_up_ope([], Obj, Ope) ->
    [{ope, [{Ope}]}|Obj];
tie_up_ope([{Key,NewKey}|Keys], Obj, Ope) ->
    case lists:keytake(Key, 1, Obj) of
        {value, {Key, null}, _} ->
            error({ope_info_notfound, Key});
        {value, {Key, Value0}, Obj1} ->
            %% all values in ope are str type
            Value = unicode:characters_to_binary(Value0, utf8, utf8),
            tie_up_ope(Keys, Obj1, [{NewKey,Value}|Ope]);
        false ->
            error({ope_info_notfound, Key})
    end.
