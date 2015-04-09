-module(dpcs_parser).

-export([parse/4]).

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
                  {ok, {Key, CommonFields0, CodeFields}} ->
                      CommonFields = case Mode of
                                         ff1 -> [];
                                         _ -> [{<<"shinym">>, Date}]
                                     end ++ CommonFields0,
                      DPCSRecord = dpcs:new(Key, Mode, CommonFields, CodeFields),
                      BinKey = dpcs:key(DPCSRecord),
                      case ets:insert_new(Table, {BinKey, DPCSRecord}) of
                          true ->
                              ok;
                          false ->
                              PrevDPCSRecord = ets:lookup_element(Table, BinKey, 2),
                              NewDPCSRecord = dpcs:merge(DPCSRecord, PrevDPCSRecord),
                              ets:insert(Table, {BinKey, NewDPCSRecord})
                      end;
                  {ok, {Key, CommonFields0}} ->
                      CommonFields = case Mode of
                                         ff1 -> [];
                                         _ -> [{<<"shinym">>, Date}]
                                     end ++ CommonFields0,
                      DPCSRecord = dpcs:new(Key, Mode, CommonFields, []),
                      BinKey = dpcs:key(DPCSRecord),
                      ets:insert_new(Table, {BinKey, DPCSRecord});
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
            F = fun({K,V}, Acc) ->
                        dpcs:add_field({K,V}, dn, Acc)
                end,
            CommonField_list = [{cocd, Cocd},{kanjaid,Kanjaid},{nyuymd,Nyuymd}],
            CommonField =  lists:foldl(F ,[], CommonField_list),

            CodeField_list = ff1_matcher:to_list(Code, Payload),
            CodeField = {list_to_binary(Code),
                         {lists:foldl(F, [], CodeField_list)}},

            Key = [Cocd, $:, Kanjaid, $:, Nyuymd],
            {ok, {Key , CommonField, [CodeField]}};
        _ ->
            {error, wrong_ff1_tokens}
    end.

%% -> {iolist(), proplists:proplist()}
parse_ff4_tokens(Tokens) ->
    case Tokens of
        [Cocd, Kanjaid,Nyuymd,Taiymd,Hokkb] ->
            F = fun({K,V}, Acc) ->
                        dpcs:add_field({K,V}, dn, Acc)
                end,
            CommonField_list = [{cocd, Cocd},{kanjaid,Kanjaid},{taiymd,Taiymd},{nyuymd,Nyuymd},{hokkb,Hokkb}],
            CommonField =  lists:foldl(F ,[], CommonField_list),
            Key = [Cocd, $:, Kanjaid, $:, Nyuymd],
            {ok, {Key, CommonField}};
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

            RececdField_list = [{undno, Undno}, {shinactnm, Shinactnm},{actten, Actten}, {actdrg,Actdrg}, {actzai, Actzai},{actcnt,Actcnt},
                                {entenkb, Entenkb}, {hokno,Hokno}, {recesyucd,Recesyucd},
                                {recptkakb,Recptkakb}, {shinkakb,Shinkakb},{drcd,Drcd},{wrdcd,Wrdcd},{wrdkb,Wrdkb}
                               ],
            CommonField_list = [{cocd, Cocd},{kanjaid,Kanjaid},{taiymd,Taiymd},{nyuymd,Nyuymd},{datakb,Datakb},{d_seqno,D_seqno},
                                {hptenmstcd,Hptenmstcd},{jisymd,Jisymd},{nyugaikb,Nyugaikb},{cotype,Cotype},
                                {dpcstaymd,Dpcstaymd},{dpcendymd, Dpcendymd}, {dpcreckymd, Dpcreckymd}, {dpccd,Dpccd}, {coefficient,Coefficient}],

            F = fun({K,V}, Acc) ->
                        dpcs:add_field({K,V}, dn, Acc)
                end,
            RececdField = {list_to_binary(Rececd) , {lists:foldl(F, [], RececdField_list)}},

            CommonField =  lists:foldl(F ,[], CommonField_list),
            Key = [Cocd, $:, Kanjaid, $:, Nyuymd, $:, Jisymd],
            {ok, {Key , CommonField, [RececdField]}};
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

            RececdField_list = [{undno, Undno}, {shindetnm, Shindetnm}, {ryo, Ryo}, { kijtani , Kijtani }, {meisaiten, Meisaiten},
                                {entenkb, Entenkb}, {jissekiten, Jissekiten}, {includekb, Includekb}, {actten, Actten}, {actdrg,Actdrg},
                                {actzai, Actzai},{actcnt,Actcnt},{hokno,Hokno}, {recesyucd,Recesyucd},
                                {recptkakb,Recptkakb}, {shinkakb,Shinkakb},{drcd,Drcd},{wrdcd,Wrdcd},{wrdkb,Wrdkb}
                               ],
            CommonField_list = [{cocd, Cocd},{kanjaid,Kanjaid},{taiymd,Taiymd},{nyuymd,Nyuymd},{datakb,Datakb},{d_seqno,D_seqno},{actdetno,Actdetno},
                                {hptenmstcd,Hptenmstcd},{jisymd,Jisymd},{nyugaikb,Nyugaikb},{cotype,Cotype}],

            F = fun({K,V}, Acc) ->
                        dpcs:add_field({K,V}, Mode, Acc)
                end,
            RececdField = {list_to_binary(Rececd) , {lists:foldl(F, [], RececdField_list)}},

            CommonField =  lists:foldl(F ,[], CommonField_list),
            Key = case Mode of
                      efn -> [Cocd, $:, Kanjaid, $:, Nyuymd, $:, Jisymd];
                      efg -> [Cocd, $:, Kanjaid, $:, Jisymd]
                  end,
            {ok, {Key , CommonField, [RececdField]}};
        _ ->
            {error, wrong_ef_tokens}
    end.
