-module(dpcs).

-include("meddatum.hrl").
-include("md_json.hrl").

-behaviour(md_record).

-export([to_json/1, from_json/1,
         key/1, bucket/1, make_2i_list/1,
         patient_id/1, hospital_id/1,
         from_file/3, from_file/4,
         check_is_set_done/2, mark_set_as_done/2,
         columns/0]).

-export([new/5, merge/2, merge_2/2, update_shinym/2,
         maybe_verify/3]).

-export([files_to_parse/1, parse_files/4]).

-type record_type() :: ff1|ff4|efg|efn|dn.

-record(dpcs, {
          key :: binary(),
          type :: record_type(),
          cocd :: binary(),
          kanjaid :: binary(),
          nyuymd :: binary(),
          shinym :: binary(),
          fields
         }).

-type rec() :: #dpcs{}.
-export_type([record_type/0, rec/0]).

-spec bucket(rec()) -> binary().
bucket(#dpcs{type = efn}) ->
    meddatum:true_bucket_name(<<?DPCS_BUCKET/binary, ":efndn">>);
bucket(#dpcs{type = efg}) ->
    meddatum:true_bucket_name(<<?DPCS_BUCKET/binary, ":efg">>);
bucket(#dpcs{type = dn}) ->
    meddatum:true_bucket_name(<<?DPCS_BUCKET/binary, ":efndn">>);
bucket(#dpcs{type = ff4}) ->
    meddatum:true_bucket_name(<<?DPCS_BUCKET/binary, ":ff">>).

-spec key(rec()) -> binary().
key(#dpcs{key=Key}) -> Key.

%% TODO
-spec make_2i_list(rec()) -> [{string(), binary()|integer()}].
make_2i_list(Rec) ->
    [{"kanjaid", patient_id(Rec)}].

-spec hospital_id(rec()) -> binary().
hospital_id(#dpcs{cocd=HospitalID}) -> HospitalID.

-spec patient_id(rec()) -> binary().
patient_id(#dpcs{kanjaid=PatientID}) -> PatientID.

-spec from_json(binary()) -> rec().
from_json(JSON) ->
    {List} = jsone:decode(JSON),
    from_json(List, #dpcs{}).

from_json([], DPCS) -> DPCS;
from_json([{<<"key">>, Key}|L], DPCS) ->
    from_json(L, DPCS#dpcs{key=Key});
from_json([{<<"type">>, Type}|L], DPCS) ->
    T = case binary_to_existing_atom(Type, utf8) of
            ff4 -> ff4;
            efg -> efg;
            efn -> efn;
            dn -> dn
        end,
    from_json(L, DPCS#dpcs{type=T});
from_json([{<<"cocd">>, V}|L], DPCS) ->
    from_json(L, DPCS#dpcs{cocd=V});
from_json([{<<"kanjaid">>, V}|L], DPCS) ->
    from_json(L, DPCS#dpcs{kanjaid=V});
from_json([{<<"nyuymd">>, V}|L], DPCS) ->
    from_json(L, DPCS#dpcs{nyuymd=V});
from_json([{<<"shinym">>, V}|L], DPCS) ->
    from_json(L, DPCS#dpcs{shinym=V});
from_json([{K, V}|L], DPCS = #dpcs{fields=F}) ->
    from_json(L, DPCS#dpcs{fields=[{K,V}|F]}).

-spec to_json(rec()) -> {ok, binary()}.
to_json(#dpcs{fields=Fields}) ->
    JSON = jsone:encode({Fields}, [native_utf8]),
    {ok, JSON}.

-spec from_file(filename:filename(), list(), pid()) -> {ok, [rec()]}.
from_file(Filename, [Mode, YYYYMM, HospitalID], Logger)
  when is_atom(Mode) ->
    Records0 = dpcs_parser:parse(Filename, Mode, YYYYMM, HospitalID, Logger),
    Records = lists:map(fun({_, Record}) -> Record end, Records0),
    {ok, Records}.


-spec from_file(filename:filename(), list(), pid(), fun()) -> {ok, rec()}.
from_file(Filename, List, Logger, _) ->
    from_file(Filename, List, Logger).

check_is_set_done(C, {HospitalID, Date}) ->
    {BT, Key} = tbk(HospitalID, Date),
    case riakc_pb_socket:get(C, BT, Key, [{pr,all}]) of
        {error, notfound} -> false;
        {ok, RiakObj} -> is_tombstone(RiakObj)
    end.

is_tombstone(RiakObj) ->
    case riakc_obj:get_contents(RiakObj) of
        [{MD,_}] ->
            TS = <<"X-Riak-Deleted">>,
            case riakc_obj:get_user_metadata_entry(MD, TS) of
                notfound -> false;
                _Value -> true
            end;
        _ ->
            error({has_siblings, {riakc_obj:bucket(RiakObj),
                                  riakc_obj:key(RiakObj)}})
    end.


%% @doc Hereby for dpcs, for a set of files that have same
%% {HospitalID, Date} is a set of import - thus if all records
%% inside it are once registered, a mark should be stored
%% in Riak. The Bucket name? That's the problem.
mark_set_as_done(C, {HospitalID, Date}) ->
    {BT, Key} = tbk(HospitalID, Date),
    RiakObject = meddatum:maybe_new_ro(C, BT, Key,
                                       ?DPCS_TRAIL_MARKER),
    riakc_pb_socket:put(C, RiakObject).

tbk(HospitalID, Date) ->
    BT = meddatum:true_bucket_name(<<?DPCS_BUCKET/binary,
                                     ?BUCKET_NAME_SEPARATOR/binary,
                                     "trail">>),
    Key = [HospitalID, ?BUCKET_NAME_SEPARATOR, Date],
    BinKey = iolist_to_binary(Key),
    {BT, BinKey}.

-spec columns() -> list().
columns() -> undefined.

%% =======

maybe_verify(Record, HospitalID, Date) ->
    case Record#dpcs.cocd of
        HospitalID -> maybe_verify(Record, Date);
        Wrong -> error({no_cocd_match, Wrong, HospitalID})
    end.             

%% @doc verify record with date specified via CUI
-spec maybe_verify(rec(), binary()) -> rec(). %error({no_date_match, binary(), string()}).
maybe_verify(#dpcs{type=ff4, fields=F} = Record, Date) ->
    maybe_verify_date_prefix(proplists:get_value(<<"taiymd">>, F), Date, Record);
maybe_verify(#dpcs{type=efg, fields=F} = Record, Date) ->
    maybe_verify_date_prefix(proplists:get_value(<<"jisymd">>, F), Date, Record);
maybe_verify(#dpcs{type=efn, fields=F} = Record, Date) ->
    maybe_verify_date_prefix(proplists:get_value(<<"jisymd">>, F), Date, Record);
maybe_verify(#dpcs{type=dn, fields=F} = Record, Date) ->
    maybe_verify_date_prefix(proplists:get_value(<<"jisymd">>, F), Date, Record).

maybe_verify_date_prefix(undefined, Date, _) -> error({no_date_match, Date});
maybe_verify_date_prefix(<<Date:6/binary, _/binary>>, Date, Record) ->  Record;
maybe_verify_date_prefix(<<"00000000">>, _, Record) -> Record; %% TODO: is this skipping really okay?
maybe_verify_date_prefix(YMD, Date, _) -> error({no_date_match, YMD, Date}).

-spec new(record_type(),
          Cocd :: binary(),
          Kanjaid :: binary(),
          Nyuymd :: binary(),
          proplists:proplist()) -> rec().
new(Type, Cocd, Kanjaid, Nyuymd, Fields0) ->
    Key = case Type of
              ff4 ->
                  iolist_to_binary([Cocd, $:, Kanjaid, $:, Nyuymd]);
              efg ->
                  Jisymd = proplists:get_value(jisymd, Fields0),
                  iolist_to_binary([Cocd, $:, Kanjaid, $:, Jisymd]);
              _ ->
                  Jisymd = proplists:get_value(jisymd, Fields0),
                  iolist_to_binary([Cocd, $:, Kanjaid, $:, Nyuymd, $:, Jisymd])
          end,
    Fields = lists:foldl(fun dpcs_parser:cleanup_fields/2, [], Fields0),

    #dpcs{key=iolist_to_binary(Key),
          type=Type,
          cocd=iolist_to_binary(Cocd),
          kanjaid=iolist_to_binary(Kanjaid),
          nyuymd=iolist_to_binary(Nyuymd),
          fields=orddict:from_list(Fields)}.

-spec update_shinym(rec(), binary()) -> rec().
update_shinym(Rec, Date) ->
    Rec#dpcs{shinym=Date}.

-spec merge([rec()], rec()) -> rec().
merge(LList, R) ->
    lists:foldl(fun merge_2/2, R, LList).

-spec merge_2(L::rec(), R::rec()) -> rec().
merge_2(L = #dpcs{key=Key, type=Type,
                  fields=LFields},
      _ = #dpcs{key=Key, type=Type,
                fields=RFields}) ->
    io:format("key: ~p~n", [Key]),
    Fields = orddict:merge(fun(ope, LV, RV) -> LV++RV;
                              (sick, LV, RV) -> LV++RV;
                              (_, V, V) -> V
                           end, LFields, RFields),
    L#dpcs{fields=Fields};
merge_2(L, R) ->
    error({cannot_merge, L, R}).

files_to_parse([Dir, HospitalID, Date]) ->
    Prefixes = [{"FF1", ff1}, {"FF4", ff4}, {"EFn", efn}, {"EFg", efg},
                {"Dn", dn}],
    {ok,
     lists:map(fun({Prefix, Mode}) ->
                       Name = [Prefix, $_, HospitalID, $_, Date, ".txt"],
                       {Mode, filename:join([Dir, lists:flatten(Name)])}
               end,
               Prefixes)};
files_to_parse([Dir, _, _|Options]) ->
    case parse_options(Options, []) of
        {ok, Files} ->
            {ok,
             lists:map(fun({File, T}) ->
                               {T, filename:join([Dir, File])}
                       end, Files)};
        {error, _} = E -> E
    end.

parse_options([], Files) -> {ok, Files};
parse_options(["-FF1", Filename|Rest], Acc) ->
    parse_options(Rest, [{Filename, ff1}|Acc]);
parse_options(["-FF4", Filename|Rest], Acc) ->
    parse_options(Rest, [{Filename, ff4}|Acc]);
parse_options(["-EFn", Filename|Rest], Acc) ->
    parse_options(Rest, [{Filename, efn}|Acc]);
parse_options(["-EFg", Filename|Rest], Acc) ->
    parse_options(Rest, [{Filename, efg}|Acc]);
parse_options(["-Dn", Filename|Rest], Acc) ->
    parse_options(Rest, [{Filename, dn}|Acc]);
parse_options(Other, _) ->
    {error, {wrong_options, Other}}.

-spec parse_files([{record_type(), filename:filename()}],
                  binary(), binary(), term()) ->
                         {ok, [{record_type(), [rec()]}]}.
parse_files(Files, HospitalID, YYYYMM, Logger) ->
    lists:foldl(
      fun({ff1, Filename}, {ok, Records0}) ->
              io:format(standard_error, "parsing ~p...~n", [Filename]),
              case dpcs_ff1:from_file(Filename, [YYYYMM, HospitalID], Logger) of
                  {ok, Records} ->
                      {ok, [{ff1, Records}|Records0]};
                  Error1 ->
                      {error, {Error1, Filename}}
              end;
         ({Mode, Filename}, {ok, Records0}) ->
              io:format(standard_error, "parsing ~p...~n", [Filename]),
              case dpcs:from_file(Filename, [Mode, YYYYMM, HospitalID], Logger) of
                  {ok, Records} ->
                      {ok, [{Mode, Records}|Records0]};
                  Error2 ->
                      {error, {Error2, Filename}}
              end;
         (_, Error) ->
              Error
      end, {ok, []}, Files).

%% -spec check_date_hospital(binary(), binary(), rec()) -> ok | {error, atom()}.
%% check_date_hospital(HospitalID, YYMM,
%%                     #dpcs{hospital_id=HospitalID,
%%                           date = YYYYMMDD} = _R)
%%   when is_binary(YYMM) ->
%%     case YYYYMMDD of
%%         <<"20", YYMM:4/binary, _/binary>> -> ok;
%%         _ ->
%%             io:format("argv:~p file:~p~n", [YYMM, YYYYMMDD]),
%%             io:format("record:~p~n", [_R]),
%%             {error, different_date}
%%     end;
%% check_date_hospital(_, Date, #dpcs{date=Date} = _) ->
%%     {error, different_hospital};
%% check_date_hospital(_H, _D, _R) ->
%%     {error, both_wrong}.

%% check_all(_, _, []) -> ok;
%% check_all(HospitalID, Date, [Record|Records]) ->
%%     case check_date_hospital(HospitalID, Date, Record) of
%%         ok -> check_all(HospitalID, Date, Records);
%%         Error -> Error
%%     end.
