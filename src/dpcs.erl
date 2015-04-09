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

-export([new/5, merge/2, merge_2/2, update_shinym/2]).

-export([files_to_parse/1, parse_files/4]).

-type record_type() :: ff1|ff4|efg|efn|dn.

-record(dpcs_common, {
          cocd :: binary(),
          kanjaid :: binary(),
          nyuymd :: binary(),
          shinym :: binary()
         }).

-record(dpcs, {
          key :: binary(),
          type :: record_type(),
          common_fields :: #dpcs_common{},
          fields :: proplists:proplist()
         }).

-type rec() :: #dpcs{}.
-export_type([record_type/0, rec/0]).

-spec bucket(rec()) -> binary().
bucket(#dpcs{type = efn}) ->
    meddatum:true_bucket_name(<<?DPCS_BUCKET/binary, "efndn">>);
bucket(#dpcs{type = efg}) ->
    meddatum:true_bucket_name(<<?DPCS_BUCKET/binary, "efg">>);
bucket(#dpcs{type = dn}) ->
    meddatum:true_bucket_name(<<?DPCS_BUCKET/binary, "efndn">>);
bucket(#dpcs{type = ff1}) ->
    meddatum:true_bucket_name(<<?DPCS_BUCKET/binary, "ff">>);
bucket(#dpcs{type = ff4}) ->
    meddatum:true_bucket_name(<<?DPCS_BUCKET/binary, "ff">>).

-spec key(rec()) -> binary().
key(#dpcs{key=Key}) -> Key.

%% TODO
-spec make_2i_list(rec()) -> [{string(), binary()|integer()}].
make_2i_list(Rec) ->
    [{"kanjaid", patient_id(Rec)}].

-spec hospital_id(rec()) -> binary().
hospital_id(#dpcs{common_fields=#dpcs_common{cocd=HospitalID}}) -> HospitalID.

-spec patient_id(rec()) -> binary().
patient_id(#dpcs{common_fields=#dpcs_common{kanjaid=PatientID}}) -> PatientID.

-spec from_json(binary()) -> rec().
from_json(JSON) ->
    {List} = jsone:decode(JSON),
    from_json(List, #dpcs{}).

from_json([], DPCS) -> DPCS;
from_json([{<<"key">>, Key}|L], DPCS) ->
    from_json(L, DPCS#dpcs{key=Key});
from_json([{<<"type">>, Type}|L], DPCS) ->
    T = case binary_to_existing_atom(Type, utf8) of
            ff1 -> ff1;
            ff4 -> ff4;
            efg -> efg;
            efn -> efn;
            dn -> dn
        end,
    from_json(L, DPCS#dpcs{type=T});
from_json([{<<"cocd">>, V}|L], DPCS = #dpcs{common_fields=CF}) ->
    from_json(L, DPCS#dpcs{common_fields=CF#dpcs_common{cocd=V}});
from_json([{<<"kanjaid">>, V}|L], DPCS = #dpcs{common_fields=CF}) ->
    from_json(L, DPCS#dpcs{common_fields=CF#dpcs_common{kanjaid=V}});
from_json([{<<"nyuymd">>, V}|L], DPCS = #dpcs{common_fields=CF}) ->
    from_json(L, DPCS#dpcs{common_fields=CF#dpcs_common{nyuymd=V}});
from_json([{<<"shinym">>, V}|L], DPCS = #dpcs{common_fields=CF}) ->
    from_json(L, DPCS#dpcs{common_fields=CF#dpcs_common{shinym=V}});
from_json([{K, V}|L], DPCS = #dpcs{fields=F}) ->
    from_json(L, DPCS#dpcs{fields=[{K,V}|F]}).

-spec to_json(rec()) -> {ok, binary()}.
to_json(#dpcs{fields=Fields, common_fields=CommonFields}) ->
    CFList = (?JSON_RECORD_OPENER(dpcs_common))(CommonFields),
    JSON = jsone:encode({CFList++Fields}, [native_utf8]),
    {ok, JSON}.

-spec from_file(filename:filename(), list(), pid()) -> {ok, [rec()]}.
from_file(Filename, [Mode, YYYYMM], Logger)
  when is_atom(Mode) ->
    Records0 = dpcs_parser:parse(Filename, Mode, YYYYMM, Logger),
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

-spec new(record_type(),
          Cocd :: binary(),
          Kanjaid :: binary(),
          Nyuymd :: binary(),
          proplists:proplist()) -> rec().
new(Type, Cocd, Kanjaid, Nyuymd, Fields0) ->
    Key = case Type of
              ff1 -> iolist_to_binary([Cocd, $:, Kanjaid, $:, Nyuymd]);
              ff4 -> iolist_to_binary([Cocd, $:, Kanjaid, $:, Nyuymd]);
              efn ->
                  Jisymd = proplists:get_value(jisymd, Fields0),
                  iolist_to_binary([Cocd, $:, Kanjaid, $:, Nyuymd, $:, Jisymd]);
              efg ->
                  Jisymd = proplists:get_value(jisymd, Fields0),
                  iolist_to_binary([Cocd, $:, Kanjaid, $:, Jisymd]);
              dn ->
                  Jisymd = proplists:get_value(jisymd, Fields0),
                  iolist_to_binary([Cocd, $:, Kanjaid, $:, Nyuymd, $:, Jisymd])
          end,
    CommonFields =
              #dpcs_common{cocd=iolist_to_binary(Cocd),
                           kanjaid=iolist_to_binary(Kanjaid),
                           nyuymd=iolist_to_binary(Nyuymd)},
    Fields = lists:foldl(fun dpcs_parser:cleanup_fields/2, [], Fields0),

    #dpcs{key=iolist_to_binary(Key),
          type=Type,
          common_fields=CommonFields,
          fields=Fields}.

-spec update_shinym(rec(), binary()) -> rec().
update_shinym(Rec = #dpcs{common_fields=CF}, Date) ->
    CommonFields = CF#dpcs_common{shinym=Date},
    Rec#dpcs{common_fields=CommonFields}.

-spec merge([rec()], rec()) -> rec().
merge(LList, R) ->
    lists:foldl(fun merge_2/2, R, LList).

-spec merge_2(L::rec(), R::rec()) -> rec().
merge_2(L = #dpcs{key=Key, type=Type,
                common_fields=CF, fields=LFields},
      _ = #dpcs{key=Key, type=Type,
                common_fields=CF, fields=RFields}) ->
    L#dpcs{fields=LFields++RFields};
merge_2(L, R) ->
    error({cannot_merge, L, R}).

files_to_parse([Dir, HospitalID, Date]) ->
    Prefixes = [{"FF1", ff1}, {"FF4", ff4}, {"EFn", efn}, {"EFg", efg},
                {"Dn", dn}],
    {ok, 
     lists:map(fun({Prefix, Mode}) ->
                       Name = [Prefix, $_, HospitalID, $_, Date, ".txt"],
                       {filename:join([Dir, lists:flatten(Name)]), Mode}
               end,
               Prefixes)};
files_to_parse([Dir, _, _|Options]) ->
    case parse_options(Options, []) of
        {ok, Files} ->
            lists:map(fun({T, File}) ->
                              {T, filename:join([Dir, File])}
                      end, Files);
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

-spec parse_files(filename:filename(), binary(), binary(), term()) ->
                         {ok, [{record_type(), [rec()]}]}.
parse_files(Files, _HospitalID, YYYYMM, Logger) ->
    lists:foldl(
      fun({Filename, Mode}, {ok, Records0}) ->
              io:format(standard_error, "parsing ~p...~n", [Filename]),
              case dpcs:from_file(Filename, [Mode, YYYYMM], Logger) of
                  {ok, Records} ->
                      %% case check_all(list_to_binary(HospitalID),
                      %%                list_to_binary(Date), Records) of
                      {ok, [{Mode, Records}|Records0]};
                      %%     Error1 -> {error, {Error1, Filename}}
                      %% end;
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
