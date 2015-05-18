-module(ff1_matcher).

-include("ff1_types.hrl").
-export([to_list/2, to_list_2/3]).

to_list("A000010", [ Birthymd, Sex, Postno, _ , _ , _ , _ , _ , _ ]) ->
    [{birthymd, Birthymd},{sex, Sex},{postno, Postno}];
to_list("A000020", [ Nyuymd, H_nyukb, Introkb, O_nyukb, E_nyukb, Emergkb, Zaitaku_befnyu, _ , _ ]) ->
    [{nyuymd, Nyuymd},{h_nyukb, H_nyukb},{introkb, Introkb},{o_nyukb, O_nyukb},{e_nyukb, E_nyukb},{emergkb, Emergkb},{zaitaku_befnyu, Zaitaku_befnyu}];
to_list("A000030", [ Taiymd, H_taikb, H_tenkkb, Death24kb, Zaitaku_afttai, _ , _ , _ , _ ]) ->
    [{taiymd, Taiymd},{h_taikb, H_taikb},{h_tenkkb, H_tenkkb},{death24kb, Death24kb},{zaitaku_afttai, Zaitaku_afttai}];
to_list("A000031", [ Form1_staymd, Form1_endymd, _ , _ , _ , _ , _ , _ , _ ]) ->
    [{form1_staymd, Form1_staymd},{form1_endymd, Form1_endymd}];
to_list("A000040", [ _ , Shinkacd, Tenkakb, _ , _ , _ , _ , _ , _ ]) ->
    [{shinkacd, Shinkacd},{tenkakb, Tenkakb}];
to_list("A000050", [ _ , C_nyukb_ippan, C_nyukb_seisin, C_nyukb_other, _ , _ , _ , _ , _ ]) ->
    [{c_nyukb_ippan, C_nyukb_ippan},{c_nyukb_seisin, C_nyukb_seisin},{c_nyukb_other, C_nyukb_other}];
to_list("A000060", [ _ , Mainexam, Curekb, _ , _ , _ , _ , _ , _ ]) ->
    [{mainexam, Mainexam},{curekb, Curekb}];
to_list("A000070", [ B_taiymd, B_nyukb, _ , _ , _ , _ , _ , _ , _ ]) ->
    [{b_taiymd, B_taiymd},{b_nyukb, B_nyukb}];
to_list("A000080", [ _ , Reent_syu, Reent_reason_syu, _ , _ , _ , _ , _ , Reent_reason]) ->
    [{reent_syu, Reent_syu},{reent_reason_syu, Reent_reason_syu},{reent_reason, Reent_reason}];
to_list("A000090", [ _ , Retransfer_syu, Retransfer_reason_syu, _ , _ , _ , _ , _ , Retransfer_reason]) ->
    [{retransfer_syu, Retransfer_syu},{retransfer_reason_syu, Retransfer_reason_syu},{retransfer_reason, Retransfer_reason}];
to_list("A001010", [ _ , Height, Weight, _ , _ , _ , _ , _ , _ ]) ->
    [{height, Height},{weight, Weight}];
to_list("A001020", [ _ , Smk_index, _ , _ , _ , _ , _ , _ , _ ]) ->
    [{smk_index, Smk_index}];
to_list("A001030", [ _ , Bedsore_nyu, Bedsore_tai, _ , _ , _ , _ , _ , _ ]) ->
    [{bedsore_nyu, Bedsore_nyu},{bedsore_tai, Bedsore_tai}];
to_list("A002010", [ _ , Pregkb, Pregweek_cnt, _ , _ , _ , _ , _ , _ ]) ->
    [{pregkb, Pregkb},{pregweek_cnt, Pregweek_cnt}];
to_list("A003010", [ _ , B_weight, Birthweek, _ , _ , _ , _ , _ , _ ]) ->
    [{b_weight, B_weight},{birthweek, Birthweek}];
to_list("A004010", [ _ , Fim_of_dep, _ , _ , _ , _ , _ , _ , _ ]) ->
    [{fim_of_dep, Fim_of_dep}];
to_list("A006010", [ _ , Mainsick_cd, _ , _ , _ , _ , _ , _ , Mainsick_nm]) ->
    [{mainsick_cd, Mainsick_cd},{mainsick_nm, Mainsick_nm}];
to_list("A006020", [ _ , Oppsick_cd, _ , _ , _ , _ , _ , _ , Oppsick_nm]) ->
    [{oppsick_cd, Oppsick_cd},{oppsick_nm, Oppsick_nm}];
to_list("A006030", [ _ , Maxsick_cd, Disaddcd, _ , _ , _ , _ , _ , Maxsick_nm]) ->
    [{maxsick_cd, Maxsick_cd},{disaddcd, Disaddcd},{maxsick_nm, Maxsick_nm}];
to_list("A006031", [ _ , Nextsick_cd, _ , _ , _ , _ , _ , _ , Nextsick_nm]) ->
    [{nextsick_cd, Nextsick_cd},{nextsick_nm, Nextsick_nm}];
to_list("A006040", [ _ , Heisick_cd, _ , _ , _ , _ , _ , _ , Heisick_nm]) ->
    [{heisick_cd, Heisick_cd},{heisick_nm, Heisick_nm}];
to_list("A006050", [ _ , Hassick_cd, _ , _ , _ , _ , _ , _ , Hassick_nm]) ->
    [{hassick_cd, Hassick_cd},{hassick_nm, Hassick_nm}];
to_list("A007010", [ Ope_opeymd, Ope_tencd, _ , Ope_cnt, Ope_side, Ope_ane, _ , _ , Ope_nm]) ->
    [{ope_opeymd, Ope_opeymd},{ope_tencd, Ope_tencd},{ope_cnt, Ope_cnt},{ope_side, Ope_side},{ope_ane, Ope_ane},{ope_nm, Ope_nm}];
to_list("A008010", [ _ , Bringing_medicine, _ , _ , _ , _ , _ , _ , _ ]) ->
    [{bringing_medicine, Bringing_medicine}];
to_list("ADL0010", [ _ , N_adlkb, _ , _ , _ , _ , _ , _ , _ ]) ->
    [{n_adlkb, N_adlkb}];
to_list("ADL0020", [ _ , T_adlkb, _ , _ , _ , _ , _ , _ , _ ]) ->
    [{t_adlkb, T_adlkb}];
to_list("CAN0010", [ _ , _ , Cnc_no, _ , _ , _ , _ , _ , _ ]) ->
    [{cnc_no, Cnc_no}];
to_list("CAN0020", [ _ , _ , Uicc_t, Uicc_n, Uicc_m, Uicc_edition, _ , _ , _ ]) ->
    [{uicc_t, Uicc_t},{uicc_n, Uicc_n},{uicc_m, Uicc_m},{uicc_edition, Uicc_edition}];
to_list("CAN0030", [ _ , _ , Cnc_stage, _ , _ , _ , _ , _ , _ ]) ->
    [{cnc_stage, Cnc_stage}];
to_list("CAN0040", [ _ , _ , Chemokb, _ , _ , _ , _ , _ , _ ]) ->
    [{chemokb, Chemokb}];
to_list("JCS0010", [ _ , N_jcskb, _ , _ , _ , _ , _ , _ , _ ]) ->
    [{n_jcskb, N_jcskb}];
to_list("JCS0020", [ _ , T_jcskb, _ , _ , _ , _ , _ , _ , _ ]) ->
    [{t_jcskb, T_jcskb}];
to_list("M010010", [ _ , N_mrs, Apoplexy_ymd, _ , _ , _ , _ , _ , _ ]) ->
    [{n_mrs, N_mrs},{apoplexy_ymd, Apoplexy_ymd}];
to_list("M010020", [ _ , T_mrs, _ , _ , _ , _ , _ , _ , _ ]) ->
    [{t_mrs, T_mrs}];
to_list("M010030", [ _ , Temodarkb, _ , _ , _ , _ , _ , _ , _ ]) ->
    [{temodarkb, Temodarkb}];
to_list("M040010", [ _ , Hg_jn_case, _ , _ , _ , _ , _ , _ , _ ]) ->
    [{hg_jn_case, Hg_jn_case}];
to_list("M040020", [ _ , Pneumonia_case, _ , _ , _ , _ , _ , _ , _ ]) ->
    [{pneumonia_case, Pneumonia_case}];
to_list("M050010", [ _ , Nyha_case, _ , _ , _ , _ , _ , _ , _ ]) ->
    [{nyha_case, Nyha_case}];
to_list("M050020", [ _ , Ccs_case, _ , _ , _ , _ , _ , _ , _ ]) ->
    [{ccs_case, Ccs_case}];
to_list("M050030", [ _ , Killip_case, _ , _ , _ , _ , _ , _ , _ ]) ->
    [{killip_case, Killip_case}];
to_list("M060010", [ _ , Child_case, _ , _ , _ , _ , _ , _ , _ ]) ->
    [{child_case, Child_case}];
to_list("M060020", [ _ , Pancreas_case, _ , _ , _ , _ , _ , _ , _ ]) ->
    [{pancreas_case, Pancreas_case}];
to_list("M070010", [ _ , Antirheumatic, _ , _ , _ , _ , _ , _ , _ ]) ->
    [{antirheumatic, Antirheumatic}];
to_list("M120010", [ _ , Childbirthkb, Childbirth_bleed, _ , _ , _ , _ , _ , _ ]) ->
    [{childbirthkb, Childbirthkb},{childbirth_bleed, Childbirth_bleed}];
to_list("M160010", [ _ , B_index, _ , _ , _ , _ , _ , _ , _ ]) ->
    [{b_index, B_index}];
to_list("M170010", [ _ , N_gafmeasure, _ , _ , _ , _ , _ , _ , _ ]) ->
    [{n_gafmeasure, N_gafmeasure}];
to_list("M170020", [ _ , Seisin_nyuform, Isolation_days, Restraint_days, _ , _ , _ , _ , _ ]) ->
    [{seisin_nyuform, Seisin_nyuform},{isolation_days, Isolation_days},{restraint_days, Restraint_days}];
to_list("Mzz0010", [ _ , Injucd, _ , _ , _ , _ , _ , _ , Injunk]) ->
    [{injucd, Injucd},{injunk, Injunk}].


to_list_2(Code, Tokens, LineNo) ->
    PayloadDef = proplists:get_value(Code, ?DPCS_CODES),
    Obj = to_object(PayloadDef, Tokens, LineNo, []),
    verify_fields(PayloadDef, Obj, LineNo).

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
        undefined -> error({required_field, LineNo, Property});
        "" -> error({required_field, LineNo, Property});
        _ -> verify_fields(Defs, Obj, LineNo)
    end.
