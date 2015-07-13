
-define(EF_FIELDS,
        [%% name, required, type
         %% 施設コード	1	str	1	cocd
         {cocd, true, str},
         %% データ識別番号	1	str	1	kanjaid
         {kanjaid, true, str},
         %% 退院年月日（西暦）	1	str	1	taiymd
         {taiymd, true, str},
         %% 入院年月日（西暦）	1	str	1	nyuymd
         {nyuymd, true, str},
         %% データ区分	1	str	1	datakb
         {datakb, true, str},
         %% 順序番号	1	str	1	d_seqno
         {d_seqno, true, str},
         %% 行為明細番号	1	str	1	actdetno
         {actdetno, true, str},
         %% 病院点数マスターコード	1	str	0	hptenmstcd
         {hptenmstcd, true, str},
         %% レセプト電算処理システム用コード	1	str	0	rececd
         {rececd, true, str},
         %% 解釈番号	0	str	0	undno
         {undno, false, str},
         %% 診療明細名称	1	str	0	shindetnm
         {shindetnm, true, str},
         %% 使用量	1	numeric	0	ryo
         {ryo, true, numeric},
         %% 基準単位	1	str	0	kijtani
         {kijtani, true, str},
         %% 明細点数	1	numeric	0	meisaiten
         {meisaiten, true, numeric},
         %% 円・点区分	1	str	0	entenkb
         {entenkb, true, str},
         %% 出来高実績点数	1	numeric	0	jissekiten
         {jissekiten, true, str},
         %% 出来高・包括フラグ	1	str	0	includekb
         {includekb, true, str},
         %% 行為点数	1	numeric	0	actten
         {actten, true, numeric},
         %% 行為薬剤料	1	numeric	0	actdrg
         {actdrg, true, numeric},
         %% 行為材料料	1	numeric	0	actzai
         {actzai, true, numeric},
         %% 行為回数	1	numeric	0	actcnt
         {actcnt, true, numeric},
         %% 保険者番号	1	str	0	hokno
         {hokno, true, str},
         %% レセプト種別コード	0	str	0	recesyucd
         {recesyucd, false, str},
         %% 実施年月日	1	str	0	jisymd
         {jisymd, true, str},
         %% レセプト科区分	1	str	0	recptkakb
         {recesyucd, true, str},
         %% 診療科区分	1	str	0	shinkakb
         {shinkakb, true, str},
         %% 医師コード	0	str	0	drcd
         {drcd, false, str},
         %% 病棟コード	0	str	0	wrdcd
         {wrdcd, false, str},
         %% 病棟区分	1	str	0	wrdkb
         {wrdkb, true, str},
         %% 入外区分	1	str	0	nyugaikb
         {nyugaikb, true, str},
         %% 施設タイプ	1	str	0	cotype
         {cotype, true, str},
         %% 診療年月	1	str	1	shinym
         {shinym, true, str}
        ]).


-define(DN_FIELDS,
        [%% name, required, type
         %% 施設コード	1	str	1	cocd
         {cocd, true, str},
         %% データ識別番号	1	str	1	kanjaid
         {kanjaid, true, str},
         %% 退院年月日（西暦）	1	str	1	taiymd
         {taiymd, true, str},
         %% 入院年月日（西暦）	1	str	1	nyuymd
         {nyuymd, true, str},
         %% データ区分	1	str	1	datakb
         {datakb, true, str},
         %% 順序番号	1	str	1	d_seqno
         {d_seqno, true, str},
         %% 病院点数マスターコード	1	str	0	hptenmstcd
         {hptenmstcd, true, str},
         %% レセプト電算処理システム用コード	1	str	0	rececd
         {rececd, true, str},
         %% 解釈番号	0	str	0	undno
         {undno, false, str},
         %% 診療明細名称	1	str	0	shindetnm
         {shindetnm, true, str},
         %% 行為点数	1	numeric	0	actten
         {actten, true, numeric},
         %% 行為薬剤料	1	numeric	0	actdrg
         {actdrg, true, numeric},
         %% 行為材料料	1	numeric	0	actzai
         {actzai, true, numeric},
         %% 行為回数	1	numeric	0	actcnt
         {actcnt, true, numeric},
         %% 保険者番号	1	str	0	hokno
         {hokno, true, str},
         %% レセプト種別コード	0	str	0	recesyucd
         {recesyucd, false, str},
         %% 実施年月日	1	str	0	jisymd
         {jisymd, true, str},
         %% レセプト科区分	1	str	0	recptkakb
         {recesyucd, true, str},
         %% 診療科区分	1	str	0	shinkakb
         {shinkakb, true, str},
         %% 医師コード	0	str	0	drcd
         {drcd, false, str},
         %% 病棟コード	0	str	0	wrdcd
         {wrdcd, false, str},
         %% 病棟区分	1	str	0	wrdkb
         {wrdkb, true, str},
         %% 入外区分	1	str	0	nyugaikb
         {nyugaikb, true, str},
         %% 施設タイプ	0	str	0	cotype
         {cotype, false, str},

         %% 算定開始日	0	str	0	dpcstaymd
         {dpcstaymd, false, str},
         %% 算定終了日	0	str	0	dpcendymd
         {dpcendymd, false, str},
         %% 診療起算日	0	str	0	dpcreckymd
         {dpcreckymd, false, str},
         %% 分類番号	0	str	0	dpccd
         {dpccd, false, str},
         %% 医療機関係数	0	numeric	0	coefficient
         {coefficient, false, numeric},

         %% 診療年月	1	str	1	shinym
         {shinym, true, str}
        ]).

-define(FF1_FIELDS,
        [
         %% 生年月日	1	str	birthymd
         {birthymd, true, str},
         %% 性別	1	str	sex
         {sex, true, str},
         %% 患者住所地域の郵便番号	1	str	postno
         {postno, true, str},
         %% 入院年月日	1	str	nyuymd
         {nyuymd, true, str},
         %% 入院経路	1	str	h_nyukb
         {h_nyukb, true, str},
         %% 他院よりの紹介の有無	0	str	introkb
         {introkb, false, str},
         %% 自院の外来からの入院	0	str	o_nyukb
         {o_nyukb, false, str},
         %% 予定・救急医療入院	0	str	e_nyukb
         {e_nyukb, false, str},
         %% 救急車による搬送の有無	0	str	emergkb
         {emergkb, false, str},
         %% 入院前の在宅医療の有無	0	str	zaitaku_befnyu
         {zaitaku_befnyu, false, str},
         %% 退院年月日	1	str	taiymd
         {taiymd, true, str},
         %% 退院先	1	str	h_taikb
         {h_taikb, true, str},
         %% 退院時転帰	1	str	h_tenkkb
         {h_tenkkb, true, str},
         %% 24 時間以内の死亡の有無	1	str	death24kb
         {death24kb, true, str},
         %% 退院後の在宅医療の有無	0	str	zaitaku_afttai
         {zaitaku_afttai, false, str},
         %% 様式1開始日	1	str	form1_staymd
         {form1_staymd, true, str},
         %% 様式1終了日	1	str	form1_endymd
         {form1_endymd, true, str},
         %% 診療科コード	1	str	shinkacd
         {shinkacd, true, str},
         %% 転科の有無	1	str	tenkakb
         {tenkakb, true, str},
         %% 調査対象となる一般病棟への入院の有無 	1	str	c_nyukb_ippan
         {c_nyukb_ippan, true, str},
         %% 調査対象となる精神病棟への入院の有無	1	str	c_nyukb_seisin
         {c_nyukb_seisin, true, str},
         %% 調査対象となるその他の病棟への入院の有無	1	str	c_nyukb_other
         {c_nyukb_other, true, str},
         %% 入院中の主な診療目的	1	str	mainexam
         {mainexam, true, str},
         %% 治験実施の有無	1	str	curekb
         {curekb, true, str},
         %% 前回退院年月日	1	str	b_taiymd
         {b_taiymd, true, str},
         %% 前回同一疾病で自院入院の有無	1	str	b_nyukb
         {b_nyukb, true, str},
         %% 再入院種別	2	str	reent_syu
         {reent_syu, true, str},
         %% 理由の種別	2	str	reent_reason_syu
         {reent_reason_syu, true, str},
         %% 自由記載欄	0	str	reent_reason
         {reent_reason, false, str},
         %% 再転棟種別	2	str	retransfer_syu
         {retransfer_syu, true, str},
         %% 理由の種別	2	str	retransfer_reason_syu
         {retransfer_reason_syu, true, str},
         %% 自由記載欄	0	str	retransfer_reason
         {retransfer_reason, false, str},
         %% 身長	1	str	height
         {height, true, str},
         %% 体重	1	str	weight
         {weight, true, str},
         %% 喫煙指数	1	numeric	smk_index
         {smk_index, true, str},
         %% 入院時の褥瘡の有無	1	str	bedsore_nyu
         {bedsore_nyu, true, str},
         %% 退院時の褥瘡の有無	1	str	bedsore_tai
         {bedsore_tai, true, str},
         %% 現在の妊娠の有無	1	str	pregkb
         {pregkb, true, str},
         %% 入院時の妊娠週数	0	numeric	pregweek_cnt
         {pregweek_cnt, false, numeric},
         %% 出生時体重	2	numeric	b_weight
         {b_weight, true, numeric},
         %% 出生時妊娠週数	2	numeric	birthweek
         {birthweek, true, numeric},
         %% 認知症高齢者の日常生活自立度判定基準	2	str	fim_of_dep
         {fim_of_dep, true, str},
         %% ICD10 コード	1	str	mainsick_cd
         {mainsick_cd, true, str},
         %% 主傷病名	1	str	mainsick_nm
         {mainsick_nm, true, str},
         %% ICD10コード	1	str	oppsick_cd
         {oppsick_cd, true, str},
         %% 入院の契機となった傷病名	1	str	oppsick_nm
         {oppsick_nm, true, str},
         %% ICD10コード	1	str	maxsick_cd
         {maxsick_cd, true, str},
         %% 病名付加コード	0	str	disaddcd
         {disaddcd, true, str},
         %% 医療資源を最も投入した傷病名	1	str	maxsick_nm
         {maxsick_nm, true, str},
         %% ICD10コード	2	str	nextsick_cd
         {nextsick_cd, true, str},
         %% 医療資源を2番目に投入した傷病名	2	str	nextsick_nm
         {nextsick_nm, true, str},
         %% ICD10コード	2	str	heisick_cd
         {heisick_cd, true, str},
         %% 入院時併存症名	2	str	heisick_nm
         {heisick_nm, true, str},
         %% ICD10コード	2	str	hassick_cd
         {hassick_cd, true, str},
         %% 入院後発症疾患名	2	str	hassick_nm
         {hassick_nm, true, str},
         %% 手術日	2	str	ope_opeymd
         {ope_opeymd, true, str},
         %% 点数表コード	2	str	ope_tencd
         {ope_tencd, true, str},
         %% 手術回数	2	str	ope_cnt
         {ope_cnt, true, str},
         %% 手術側数	2	str	ope_side
         {ope_side, true, str},
         %% 麻酔	2	str	ope_ane
         {ope_ane, true, str},
         %% 手術名	2	str	ope_nm
         {ope_nm, true, str},
         %% 持参薬の使用の有無	0	str	bringing_medicine
         {bringing_medicine, true, str},
         %% 入院時のADLスコア	0	str	n_adlkb
         {n_adlkb, true, str},
         %% 退院時のADLスコア	0	str	t_adlkb
         {t_adlkb, true, str},
         %% がんの初発、再発	2	str	cnc_no
         {cnc_no, true, str},
         %% UICC病期分類(T)	2	str	uicc_t
         {uicc_t, true, str},
         %% UICC病期分類(N)	2	str	uicc_n
         {uicc_n, true, str},
         %% UICC病期分類(M)	2	str	uicc_m
         {uicc_m, true, str},
         %% UICC病期分類(版)	2	str	uicc_edition
         {uicc_edition, true, str},
         %% 癌取扱い規約に基づくがんのStage分類	2	str	cnc_stage
         {cnc_stage, true, str},
         %% 化学療法の有無	2	str	chemokb
         {chemokb, true, str},
         %% 入院時意識障害がある場合のJCS	2	str	n_jcskb
         {n_jcskb, true, str},
         %% 退院時意識障害がある場合のJCS	2	str	t_jcskb
         {t_jcskb, true, str},
         %% 発症前 Rankin Scale	2	str	n_mrs
         {n_mrs, true, str},
         %% 脳卒中の発症時期	2	str	apoplexy_ymd
         {apoplexy_ymd, true, str},
         %% 退院時 modified Rankin Scale	2	str	t_mrs
         {t_mrs, true, str},
         %% テモゾロミド（初回治療）の有無	2	str	temodarkb
         {temodarkb, true, str},
         %% Hugh-Jones分類	2	str	hg_jn_case
         {hg_jn_case, true, str},
         %% 肺炎の重症度分類	2	str	pneumonia_case
         {pneumonia_case, true, str},
         %% 心不全のNYHA心機能分類	2	str	nyha_case
         {nyha_case, true, str},
         %% 狭心症、慢性虚血性心疾患（050050）における入院時の重症度：CCS分類	2	         str	ccs_case
         {ccs_case, true, str},
         %% 急性心筋梗塞（050030）における入院時の重症度：Killip分類	2	str	killip_case
         {killip_case, true, str},
         %% 肝硬変のChild-Pugh分類	2	str	child_case
         {child_case, true, str},
         %% 急性膵炎の重症度分類	2	str	pancreas_case
         {pancreas_case, true, str},
         %% 抗リウマチ分子標的薬の初回導入治療の有無	2	str	antirheumatic
         {antirheumatic, true, str},
         %% 入院周辺の分娩の有無	2	str	childbirthkb
         {childbirthkb, true, str},
         %% 分娩時出血量	0	str	childbirth_bleed
         {childbirth_bleed, true, str},
         %% BurnIndex	2	numeric	b_index
         {b_index, true, numeric},
         %% 入院時GAF尺度	2	str	n_gafmeasure
         {n_gafmeasure, true, str},
         %% 精神保健福祉法における入院形態	2	str	seisin_nyuform
         {seisin_nyuform, true, str},
         %% 精神保健福祉法に基づく隔離日数	2	numeric	isolation_days
         {isolation_days, true, numeric},
         %% 精神保健福祉法に基づく身体拘束日数	2	numeric	restraint_days
         {restraint_days, true, numeric},
         %% その他の重症度分類・分類番号または記号	2	str	injucd
         {injucd, true, str},
         %% その他の重症度分類・名称	2	str	injunk
         {injunk, true, str}

        ]).
