-module(dpcs).

-include("meddatum.hrl").

-behaviour(md_record).

-export([to_json/1, from_json/1,
         key/1, bucket/1, make_2i_list/1,
         patient_id/1, hospital_id/1,
         from_file/3, from_file/4,
         columns/0]).

-export([new/4, merge/2, add_field/3]).

-type record_type() :: ff1|ff4|efg|efn|dn.

-record(dpcs, {
          key :: binary(),
          type :: record_type(),
          hospital_id :: binary(),
          patient_id :: binary(),
          fields :: proplists:proplist(),
          common_fields :: proplists:proplist()}).

-type rec() :: #dpcs{}.
-export_type([record_type/0, rec/0]).

-spec bucket(rec()) -> binary().
bucket(#dpcs{type = Type, hospital_id = HospitalID}) ->
    meddatum:true_bucket_name(iolist_to_binary([klib:maybe_a2b(Type), $:, HospitalID])).

-spec key(rec()) -> binary().
key(#dpcs{key=Key}) -> Key.

%% TODO
-spec make_2i_list(rec()) -> [{string(), binary()|integer()}].
make_2i_list(#dpcs{}) ->
    undefined.

-spec hospital_id(rec()) -> binary().
hospital_id(#dpcs{hospital_id=HospitalID}) -> HospitalID.

-spec patient_id(rec()) -> binary().
patient_id(#dpcs{patient_id=PatientID}) -> PatientID.

-spec from_json(binary()) -> rec().
from_json(_JSON) -> undefined.

-spec to_json(rec()) -> {ok, binary()}.
to_json(#dpcs{fields=Fields, common_fields=CommonFields}) ->
    JSON = jsone:encode({CommonFields++Fields}, [native_utf8]),
    {ok, JSON}.

-spec from_file(filename:filename(), list(), pid()) -> {ok, [rec()]}.
from_file(Filename, [Mode], Logger) when is_atom(Mode) ->
    Records0 = dpcs_parser:parse(Filename, Mode, Logger),
    Records = lists:map(fun({_, Record}) -> Record end, Records0),
    {ok, Records}.


-spec from_file(filename:filename(), list(), pid(), fun()) -> {ok, rec()}.
from_file(Filename, List, Logger, _) ->
    from_file(Filename, List, Logger).

-spec columns() -> list().
columns() -> undefined.

%% =======

-spec new(iolist(), record_type(), proplists:proplist(), list(tuple())) -> rec().
new(Key, Type, CommonFields, CodeField) ->
    PatientID = proplists:get_value(<<"kanjaid">>, CommonFields),
    HospitalID = proplists:get_value(<<"cocd">>, CommonFields),
    #dpcs{key=iolist_to_binary(Key),
          patient_id=PatientID,
          hospital_id=HospitalID,
          type=Type,
          common_fields=CommonFields,
          fields=[CodeField]}.

-spec merge(New::rec(), Old::rec()) -> rec().
merge(New = #dpcs{fields=NewFields}, _ = #dpcs{fields=Fields}) ->
    New#dpcs{fields=NewFields++Fields}.

%% Rnext = [CodeField | R],
-spec add_field({atom(), binary()}, dpcs:record_type(),
                [{binary(),integer()|binary()}]) ->
                       [{binary(), integer()|binary()}].
add_field({FieldName , FieldValue} , Mode, Fields) ->
    case trans_field({FieldName, FieldValue}, Mode) of
        [] -> Fields;
        {K,V} -> [{K,V} | Fields]
    end.

-spec trans_field({atom(), string()}, dpcs:record_type()) -> [] | {binary(), integer()|binary()}.
trans_field({FieldName, FieldValue}, Mode) ->
    VTrim = string:strip(FieldValue),
    case VTrim of
        [] -> [];
        _  -> case is_numeric_field(FieldName, Mode) of
                  true ->  {atom_to_binary(FieldName,utf8), str_to_num(VTrim)};
                  false -> {atom_to_binary(FieldName,utf8), unicode:characters_to_binary(VTrim,utf8,utf8)}
        end
    end.

-spec str_to_num(string()) -> integer() | float().
str_to_num(Str) ->
    %% if decimal point omits from number, number is regarded as a integer.
    case string:chr(Str , $.) of
        0 -> list_to_integer(Str);
        _ -> list_to_float(Str)
    end.

-spec is_numeric_field(atom(), any()) -> boolean().
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
