-module(dpcs).

-export([addField/3]).

addField({FieldName , FieldValue} , Mode, Fields) ->
    case trans_field({FieldName, FieldValue}, Mode) of
        [] -> Fields;
        {K,V} -> [{K,V} | Fields]
    end.

trans_field({FieldName, FieldValue}, Mode) ->
    VTrim = string:strip(FieldValue),
    case VTrim of
        [] -> [];
        _  -> case is_numeric_field(FieldName, Mode) of
                  true ->  {atom_to_binary(FieldName,utf8), str_to_num(VTrim)};
                  false -> {atom_to_binary(FieldName,utf8), unicode:characters_to_binary(VTrim,utf8,utf8)}
        end
    end.

str_to_num(Str) -> 
    %% if decimal point omits from number, number is regarded as a integer.
    case string:chr(Str , $.) of
        0 -> list_to_integer(Str);
        _ -> list_to_float(Str)
    end.

is_numeric_field(ryo, _) -> true;
is_numeric_field(meisaiten, _) -> true;
is_numeric_field(jissekiten, _) -> true;
is_numeric_field(actten, _) -> true;
is_numeric_field(actdrg, _) -> true;
is_numeric_field(actzai, _) -> true;
is_numeric_field(actcnt, _) -> true;
is_numeric_field(coefficient, _) -> true;
is_numeric_field(smk_index, _) -> true;
is_numeric_field(pregweek_cnt , _) -> true;
is_numeric_field(b_weight , _) -> true;
is_numeric_field(birthweek, _) -> true;
is_numeric_field(b_index , _) -> true;
is_numeric_field(isolation_days , _) -> true;
is_numeric_field(restraint_days , _) -> true;
is_numeric_field(_, _) -> false.

