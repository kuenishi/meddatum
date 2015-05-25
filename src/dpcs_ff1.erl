-module(dpcs_ff1).

-include("meddatum.hrl").
-include("md_json.hrl").
-incluce_lib("eunit/include/eunit.hrl").

-behaviour(md_record).

-export([to_json/1, from_json/1,
         key/1, bucket/1, make_2i_list/1,
         patient_id/1, hospital_id/1,
         from_file/3, from_file/4, merge/2,
         check_is_set_done/2, mark_set_as_done/2,
         columns/0]).

-record(dpcs_ff1, {
          key :: binary(),
          cocd :: binary(),
          kanjaid :: binary(),
          nyuymd :: binary(),
          taiymd :: binary(),
          shinym :: binary(),
          stay = null :: {orddict:orddict()}, %% 親様式1
          wards = [] :: [{orddict:orddict()}] %% 子様式1
         }).

-record(ctx,
        {
          line_no = 1 :: non_neg_integer(),
          current = #dpcs_ff1{} :: #dpcs_ff1{},
          current_kaisukanrino,
          current_medicalno,
          current_fields = [],
          records = [] :: [#dpcs_ff1{}]
        }).

-spec bucket(#dpcs_ff1{}) -> binary().
bucket(#dpcs_ff1{} = _) ->
    meddatum:true_bucket_name(<<?DPCS_BUCKET/binary, ":ff">>).

-spec key(#dpcs_ff1{}) -> binary().
key(#dpcs_ff1{key=Key}) -> Key.

-spec make_2i_list(#dpcs_ff1{}) -> [{string(), binary()|integer()}].
make_2i_list(Rec) ->
    [{"kanjaid", patient_id(Rec)}].

-spec hospital_id(#dpcs_ff1{}) -> binary().
hospital_id(#dpcs_ff1{cocd=HospitalID}) -> HospitalID.

-spec patient_id(#dpcs_ff1{}) -> binary().
patient_id(#dpcs_ff1{kanjaid=PatientID}) -> PatientID.

-spec from_json(binary()) -> #dpcs_ff1{}.
from_json(JSON) ->
    Decoder = ?JSON_RECORD_DECODER(dpcs_ff1),
    Decoder(JSON).

-spec to_json(#dpcs_ff1{}) -> {ok, binary()}.
to_json(DPCS) ->
    Encoder = ?JSON_RECORD_ENCODER(dpcs_ff1),
    {ok, Encoder(DPCS)}.

-spec from_file(filename:filename(), list(), pid()) -> {ok, [#dpcs_ff1{}]}.
from_file(Filename, [YYYYMM, _HospitalID], _Logger) ->
    F = fun(Line, Ctx) ->  parse_line(Line, YYYYMM, Ctx) end,
    {ok, Ctx1} = japanese:fold_all_lines(Filename, F, #ctx{}),
    #ctx{records=Records, line_no=_LineNo} = Ctx1,
    {ok, lists:reverse(Records)}.

-spec from_file(filename:filename(), list(), pid(), fun()) -> {ok, #dpcs_ff1{}}.
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


parse_line(Line, Date, #ctx{
                      current_medicalno = undefined,
                      current_kaisukanrino = undefined,
                      records = [],
                      line_no=LineNo} = Ctx) ->
    Tokens = re:split(Line, "[\t]", [{return, list}, unicode]),
    [Cocd, Kanjaid, Nyuymd, Kaisukanrino, MedicalNo, Code ,
     _Version, _Seqno | Payload] = Tokens,
    Fields1 = ff1_matcher:to_list(Code, Payload, LineNo),
    Fields = lists:foldl(fun dpcs_parser:cleanup_fields/2, [], Fields1),
    Ctx#ctx{
      current=new(Cocd, Kanjaid, Nyuymd, Date, Kaisukanrino),
      current_fields=Fields,
      current_kaisukanrino=Kaisukanrino,
      current_medicalno=MedicalNo,
      line_no=LineNo+1};

parse_line(Line, Date, #ctx{current=Current,
                            %% current_kaisukanrino = Kaisukanrino0,
                            current_medicalno = MedicalNo0,
                            current_fields = Fields0,
                            records = Records0,
                            line_no=LineNo} = Ctx) ->
    Tokens = re:split(Line, "[\t]", [{return, list}, unicode]),
    #dpcs_ff1{
       cocd=Cocd0Str, kanjaid=Kanjaid0Str, nyuymd=Nyuymd0Str,
       wards=Wards0} = Current,
    %% io:format("~p: ~s~n", [LineNo, dpcs_ff1:to_json(Current)]),
    Cocd0 = binary_to_list(Cocd0Str),
    Kanjaid0 = binary_to_list(Kanjaid0Str),
    Nyuymd0 = binary_to_list(Nyuymd0Str),
    %% io:format("~p <=> ~p~n", [{Cocd0, Kanjaid0, Nyuymd0}, Tokens]),
    C=case Tokens of
          %% All known but new line; just add to stay
          [Cocd0, Kanjaid0, Nyuymd0, "0", MedicalNo0, Code ,
           _Version, _Seqno | Payload] ->
              Fields1 = ff1_matcher:to_list(Code, Payload, LineNo),
              Fields = lists:foldl(fun dpcs_parser:cleanup_fields/2, [], Fields1),
              Ctx#ctx{current_fields=merge_fields(Fields, Fields0)};

          %% New MedicalNo - new one but same key
          [Cocd0, Kanjaid0, Nyuymd0, "0", MedicalNo, Code ,
           _Version, _Seqno | Payload] ->
              Fields1 = ff1_matcher:to_list(Code, Payload, LineNo),
              Fields = lists:foldl(fun dpcs_parser:cleanup_fields/2, [], Fields1),
              DPCS = case MedicalNo0 of
                         "0" ->
                             Current#dpcs_ff1{stay={orddict:from_list(Fields0)}};
                         S when S =:= "A" orelse "B" ->
                             #dpcs_ff1{};
                         _ -> %% 1 ... 9
                             Wards = [{[{medical_no,
                                         list_to_binary(MedicalNo0)}|Fields0]}
                                      |Wards0],
                             Current#dpcs_ff1{wards=lists:reverse(Wards)}
                     end,
              Ctx#ctx{current=DPCS,
                      current_fields=Fields,
                      current_medicalno=MedicalNo};

          %% New Kaisukanrino - not a case now, just ignore
          %% [Cocd0, Kanjaid0, Nyuymd0, Kaisukanrino, MedicalNo, Code ,
          %%  _Version, _Seqno | Payload] ->

          %% New Cocd, Kanjaid, or Nyuymd: should be a new record
          [Cocd, Kanjaid, Nyuymd, Kaisukanrino, MedicalNo, Code ,
           _Version, _Seqno | Payload] ->
              Fields1 = ff1_matcher:to_list(Code, Payload, LineNo),
              Fields = lists:foldl(fun dpcs_parser:cleanup_fields/2, [], Fields1),
              RecordToAdd = case MedicalNo0 of
                                "0" ->
                                    [finalize(Current#dpcs_ff1{stay={Fields0}})];
                                S when S =:= "A" orelse "B" ->
                                    [];
                                _ -> %% 1 ... 9
                                    Wards = [{[{medical_no,
                                                list_to_binary(MedicalNo0)}|Fields0]}
                                             |Wards0],
                                    [finalize(Current#dpcs_ff1{wards=Wards})]
                            end,

              Ctx#ctx{
                current=new(Cocd, Kanjaid, Nyuymd, Date, Kaisukanrino),
                current_fields=Fields,
                current_kaisukanrino=Kaisukanrino,
                current_medicalno=MedicalNo,
                records=RecordToAdd ++ Records0}
    end,

    C#ctx{line_no = LineNo+1}.

-spec new(Cocd :: iolist(),
          Kanjaid :: iolist(),
          Nyuymd :: iolist(),
          Date::binary(),
          Kaisukanrino :: iolist()) -> #dpcs_ff1{}.
new(Cocd, Kanjaid, Nyuymd, Date, Kaisukanrino) ->
    Key = iolist_to_binary([Cocd, $:, Kanjaid, $:, Nyuymd, $:, Kaisukanrino]),
    #dpcs_ff1{key=iolist_to_binary(Key),
              cocd=iolist_to_binary(Cocd),
              kanjaid=iolist_to_binary(Kanjaid),
              nyuymd=iolist_to_binary(Nyuymd),
              shinym=Date}.

finalize(DPCS = #dpcs_ff1{stay={Stay}, shinym=Date}) ->
    Taiymd = case proplists:get_value(taiymd, Stay) of
                 undefined ->
                     case proplists:get_value(<<"taiymd">>, Stay) of
                         undefined -> error({no_taiymd, Stay});
                         T1 ->        T1
                     end;
                 T ->
                     T
             end,
    dpcs:maybe_verify_date_prefix(Taiymd, Date, DPCS),
    DPCS#dpcs_ff1{taiymd=Taiymd};
finalize(DPCS = #dpcs_ff1{stay=null, wards=_Wards}) ->
    %% TODO: Error path ...
    %% lists:foldl(fun(DPCS = #dpcs_ff1{stay=null, wards=[H|T]}, AccWards) ->
    %%                     case proplists:get_value(taiymd, H) of
    %%                         undefined -> [H|AccWards]
    %% Taiymd -> {ok, DPCS#dpcs_ff1{
    DPCS#dpcs_ff1{stay=null}.


-spec merge(#dpcs_ff1{}, #dpcs_ff1{}) -> #dpcs_ff1{}.
merge(_, _) ->
    fail.

-spec merge_fields(orddict:orddict(), orddict:orddict()) -> orddict:orddict().
merge_fields(LFields, RFields) ->
    orddict:merge(fun(ope, LV, RV) -> LV++RV;
                     (sick, LV, RV) -> LV++RV;
                     (_, V, V) -> V
                  end, LFields, RFields).
