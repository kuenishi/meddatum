-module(dpcs_parser).

-export([parse/4, cleanup_fields/2]).

-include_lib("eunit/include/eunit.hrl").

-spec parse(filename:filename(), dpcs:record_type(),
            binary(), pid()) ->[dpcs:rec()].
parse(Filename, Mode, Date, Logger) ->
    {ok, Lines0} = japanese:read_file(Filename),
    Lines = lists:zip(lists:seq(1, length(Lines0)), Lines0),
    Table = ets:new(ef_data, [set, private]),
    lists:foreach(
      fun({_, [10]}) ->
              %% End of file last line ignored
              ok;
         ({LineNo, Line}) ->
              StripLine = case Line of
                              [$\n|L] -> L;
                              L -> L
                          end,
              Tokens = re:split(StripLine, "[\t]", [{return, list}, unicode]),
              case parse_tokens(Tokens, Mode) of
                  {ok, DPCSRecord0} ->
                      DPCSRecord = dpcs:update_shinym(DPCSRecord0, Date),
                      BinKey = dpcs:key(DPCSRecord),
                      case ets:insert_new(Table, {{Mode, BinKey}, DPCSRecord}) of
                          true ->
                              ok;
                          false ->
                              PrevDPCSRecord = ets:lookup_element(Table, {Mode, BinKey}, 2),
                              NewDPCSRecord = dpcs:merge(DPCSRecord, PrevDPCSRecord),
                              ets:insert(Table, {{Mode, BinKey}, NewDPCSRecord})
                      end;
                  Error ->
                      treehugger:hug(Logger, error, "Invalid format at ~s line ~p: ~p",
                                     [Filename, LineNo, Error]),
                      error({Filename, LineNo, Error})
              end
      end,
      Lines),
    ets:tab2list(Table).

-spec parse_tokens([string()], dpcs:record_type()) ->
                          {ok, {iolist(), term(), term()}} |
                          {ok, {iolist(), term()}} |
                          {error, atom()}.
parse_tokens(Tokens, ff1) -> parse_ff1_tokens(Tokens);
parse_tokens(Tokens, ff4) -> parse_ff4_tokens(Tokens);
parse_tokens(Tokens, dn) -> parse_dn_tokens(Tokens);
parse_tokens(Tokens, efg) -> parse_ef_tokens(Tokens, efg);
parse_tokens(Tokens, efn) -> parse_ef_tokens(Tokens, efn).

%% -> {iolist(), proplists:proplist(), {binary(), {[...]}}}
-spec parse_ff1_tokens([string()]) ->
                              {ok, {iolist(), proplists:proplist(),
                                    proplists:proplist()}} |
                              {error, wrong_ff1_tokens}.
parse_ff1_tokens(Tokens) ->
    case Tokens of
        [Cocd, Kanjaid, Nyuymd, _Kaisukanrino, _Medical_no, Code , _Version, _Seqno | Payload] ->
            CodeFields = ff1_matcher:to_list(Code, Payload),
            {ok, dpcs:new(ff1, Cocd, Kanjaid, Nyuymd, CodeFields)};
        _ ->
            {error, wrong_ff1_tokens}
    end.

%% -> {iolist(), proplists:proplist()}
parse_ff4_tokens(Tokens) ->
    case Tokens of
        [Cocd, Kanjaid,Nyuymd,Taiymd,Hokkb] ->
            Fields = [{taiymd,Taiymd},{hokkb,Hokkb}],
            {ok, dpcs:new(ff4, Cocd, Kanjaid, Nyuymd,Fields)};
        _ ->
            {error, wrong_ff4_tokens}
    end.

%% -> {iolist(), proplists:proplist(), {rececode, {[]}}}
parse_dn_tokens(Tokens) ->
    case Tokens of
        [Cocd ,Kanjaid ,Taiymd ,Nyuymd ,Datakb ,D_seqno ,Hptenmstcd ,Rececd,
         Undno ,Shinactnm ,Actten ,Actdrg ,Actzai ,Entenkb ,Actcnt ,Hokno ,Recesyucd,
         Jisymd,
         Recptkakb ,Shinkakb ,Drcd ,Wrdcd ,Wrdkb,
         Nyugaikb ,Cotype ,Dpcstaymd ,Dpcendymd ,Dpcreckymd ,Dpccd ,Coefficient] ->

            Fields = [{undno, Undno}, {shinactnm, Shinactnm},{actten, Actten}, {actdrg,Actdrg}, {actzai, Actzai},{actcnt,Actcnt},
                      {entenkb, Entenkb}, {hokno,Hokno}, {recesyucd,Recesyucd},
                      {recptkakb,Recptkakb}, {shinkakb,Shinkakb},{drcd,Drcd},{wrdcd,Wrdcd},{wrdkb,Wrdkb},
                      {taiymd,Taiymd},{datakb,Datakb},{d_seqno,D_seqno},{rececd, Rececd},
                      {hptenmstcd,Hptenmstcd},{jisymd,Jisymd},{nyugaikb,Nyugaikb},{cotype,Cotype},
                      {dpcstaymd,Dpcstaymd},{dpcendymd, Dpcendymd}, {dpcreckymd, Dpcreckymd}, {dpccd,Dpccd}, {coefficient,Coefficient}],
            
            {ok, dpcs:new(dn, Cocd, Kanjaid, Nyuymd, Fields)};
        _ ->
            {error, wrong_dn_tokens}
    end.

parse_ef_tokens(Tokens, Mode) ->
    case Tokens of
        [Cocd, Kanjaid, Taiymd, Nyuymd, Datakb, D_seqno, Actdetno, Hptenmstcd, Rececd ,
         Undno, Shindetnm, Ryo, Kijtani, Meisaiten, Entenkb, Jissekiten, Includekb, Actten, Actdrg, Actzai, Actcnt, Hokno, Recesyucd,
         Jisymd,
         Recptkakb,Shinkakb,Drcd, Wrdcd, Wrdkb,
         Nyugaikb, Cotype] ->

            Fields = [{undno, Undno}, {shindetnm, Shindetnm}, {ryo, Ryo}, { kijtani , Kijtani }, {meisaiten, Meisaiten},
                      {entenkb, Entenkb}, {jissekiten, Jissekiten}, {includekb, Includekb}, {actten, Actten}, {actdrg,Actdrg},
                      {actzai, Actzai},{actcnt,Actcnt},{hokno,Hokno}, {recesyucd,Recesyucd},
                      {recptkakb,Recptkakb}, {shinkakb,Shinkakb},{drcd,Drcd},{wrdcd,Wrdcd},{wrdkb,Wrdkb},
                      {taiymd,Taiymd},{datakb,Datakb},{d_seqno,D_seqno},{actdetno,Actdetno},
                      {hptenmstcd,Hptenmstcd},{rececd, Rececd}, {jisymd,Jisymd},{nyugaikb,Nyugaikb},{cotype,Cotype}],

            {ok, dpcs:new(Mode, Cocd, Kanjaid, Nyuymd, Fields)};
        _ ->
            {error, wrong_ef_tokens}
    end.

-spec cleanup_fields({atom(), binary()},
                    [{binary(),integer()|binary()}]) ->
                           [{binary(), integer()|binary()}].
cleanup_fields({FieldName, FieldValue}, Fields) ->
    case string:strip(FieldValue) of
        "" ->
            Fields;
        TrimmedValue ->
            [trans_field({FieldName, TrimmedValue})|Fields]
    end.

-spec trans_field({atom(), string()}) ->
                         {binary(), float()|integer()|binary()}.
trans_field({FieldName, FieldValue}) when is_atom(FieldName) ->
    case is_numeric_field(FieldName) of
        true ->
            {atom_to_binary(FieldName,utf8),
             klib:str_to_numeric(FieldValue)};
        false ->
            {atom_to_binary(FieldName,utf8),
             unicode:characters_to_binary(FieldValue,utf8,utf8)}
    end;
trans_field(F) -> F.


-spec is_numeric_field(atom()) -> boolean().
is_numeric_field(ryo) -> true;
is_numeric_field(meisaiten) -> true;
is_numeric_field(jissekiten) -> true;
is_numeric_field(actten) -> true;
is_numeric_field(actdrg) -> true;
is_numeric_field(actzai) -> true;
is_numeric_field(actcnt) -> true;
is_numeric_field(coefficient) -> true;
is_numeric_field(smk_index) -> true;
is_numeric_field(pregweek_cnt ) -> true;
is_numeric_field(b_weight ) -> true;
is_numeric_field(birthweek) -> true;
is_numeric_field(b_index ) -> true;
is_numeric_field(isolation_days ) -> true;
is_numeric_field(restraint_days ) -> true;
is_numeric_field(_) -> false.

