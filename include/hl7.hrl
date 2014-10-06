%% @doc when changing this record, make sure that you're also editing
%% hl7:schema/0.
-record(hl7msg, {
          hospital_id  :: binary(),
          patient_id  :: binary(),
          file :: filename:filename(),
          date :: string(),
          msg_type_s :: string(),
          msg_id :: string(),
          segments = [] :: list({list()})
         }).

%% define primary records here, type/specs are in hl7.erl
-type user_defined_code() :: string().
-type hl7_defined_code() :: string().

%% undefined in the excel:
-type 'FN'() :: string(). %% JAHIS臨床検査データ交換規約3.1では構造を持ったものとして定義されている
-type 'SAD'() :: string(). %% JAHIS臨床検査データ交換規約3.1では構造を持ったものとして定義されている
-type 'GTS'() :: string().

%% データ型	型の説明	成分
-type 'ST'() :: string(). %%文字列データ	-
-type 'TX'() :: string(). %%テキストデータ	-
-type 'FT'() :: string(). %%書式付テキストデータ	-
-type 'NM'() :: integer()|float(). %%数値	-
-type 'IS'() :: user_defined_code(). %%使用者定義表コード化値	-
-type 'ID'() :: hl7_defined_code(). %%HL7定義表コード化値	-

%%HD 	階層的指定子	<Namespace ID (IS)> ^ <Universal ID (ST)> ^ <Universal ID Type (ID)>	namespace_id	univ_id	univ_id_type
-define(HL7_HD, [
          {namespace_id , 'IS'},
          {univ_id      , 'ST'},
          {univ_id_type , 'ID'}
         ]).

%% CE	コード化値	<Identifier (ST)> ^ <Text (ST)> ^ <Name of Coding System (IS)> ^ <Alternate Identifier (ST)> ^ <Alternate Text (ST)> ^ <Name of Alternate Coding System (IS)>	id	txt	nm_cs	id_alt	txt_alt	nm_cs_alt
-define(HL7_CE, [
          {id       , 'ST'},
          {txt      , 'ST'},
          {nm_cs    , 'IS'},
          {id_alt   , 'ST'},
          {txt_alt  , 'ST'},
          {nm_cs_alt, 'IS'}
         ]).

%% CNE	拡張なしのコード化値	<Identifier (ST)> ^ <Text (ST)> ^ <Name of Coding System (IS)> ^ <Alternate Identifier (ST)> ^ <Alternate Text (ST)> ^ <Name of Alternate Coding System (IS)> ^ <Coding System Version ID (ST)> ^ <Alternate Coding System Version ID (ST)> ^ <Original Text (ST)>	id	txt	nm_cs	id_alt	txt_alt	nm_cs_alt	cs_ver_id	cs_ver_id_alt	orgin_txt
-define(HL7_CNE, [
          {id         , 'ST'},
          {txt        , 'ST'},
          {nm_cs      , 'IS'},
          {id_alt     , 'ST'},
          {txt_alt    , 'ST'},
          {nm_cs_alt  , 'IS'},
          {cs_ver_id  , 'ST'},
          {cs_ver_id_alt	, 'ST'},
          {orgin_txt  , 'ST'}
         ]).

%% CWE	拡張ありのコード化値	<Identifier (ST)> ^ <Text (ST)> ^ <Name of Coding System (IS)> ^ <Alternate Identifier (ST)> ^ <Alternate Text (ST)> ^ <Name of Alternate Coding System (IS)> ^ <Coding System Version ID (ST)> ^ <Alternate Coding System Version ID (ST)> ^ <Original Text (ST)>	id	txt	nm_cs	id_alt	txt_alt	nm_cs_alt	cs_ver_id
-define(HL7_CWE, [
          {id         , 'ST'},
          {txt        , 'ST'},
          {nm_cs      , 'IS'},
          {id_alt     , 'ST'},
          {txt_alt    , 'ST'},
          {nm_cs_alt  , 'IS'},
          {cs_ver_id  , 'ST'},
          {cs_ver_id_alt	, 'ST'},
          {orgin_txt  , 'ST'}
         ]).

-type 'DT'() :: string(). %%日付	-
-type 'TM'() :: string(). %%時間	-
-type 'DTM'() :: string(). %%日付／時刻	-

%% TS	タイムスタンプ	<Time (DTM)> ^ <DEPRECATED-Degree of Precision (ID)>
-define(HL7_TS, [
          {time  , 'DTM'},
          {precision , 'ID'}
         ]).

%% DR	日付／時間の範囲	<Range Start Date/Time (TS)> ^ <Range End Date/Time (TS)>
-define(HL7_DR, [
          {time_start , 'TS'},
          {time_end   , 'TS'}
         ]).

%% MSG	メッセージ型	<Message type (ID)> ^ <Trigger Event (ID)> ^ <Message Structure(ID)>
-define(HL7_MSG, [
          {msg_type     , 'ID'},
          {trigger_evnt , 'ID'},
          {msg_structure, 'ID'}
         ]).

%% PT	処理型	<Processing ID (ID)> ^<Processing Mode (ID)>
-define(HL7_PT, [
          {proc_id   , 'ID'},
          {proc_md   , 'ID'}
         ]).

%% VID	バージョン識別子	<Version ID (ID)> ^ <Internationalization Code (CWE)> ^ <International Version ID (CWE)>
-define(HL7_VID, [
          {ver_id      , 'ID'},
          {inter_cd    , 'CWE'},
          {inter_ver_id, 'CWE'}
         ]).

%% XCN	拡張複合IDと名前	"<ID Number (ST)> ^ <Family Name (FN)> ^ <Given Name (ST)> ^ <Second and FurtherGiven Names or Initials Thereof (ST)> ^ <Suffix (e.g., JR or III) (ST)> ^ <Prefix (e.g., DR)(ST)> ^ <Degree (e.g., MD) (IS)> ^ <Source Table (IS)> ^ <Assigning Authority (HD)> ^<Name Type Code (ID)> ^ <Identifier Check Digit (ST)> ^ <Check Digit Scheme (ID)> ^<Identifier Type Code (ID)> ^ <Assigning Facility (HD)> ^ <Name Representation Code(ID)> ^ <Name Context (CE)> ^ <Name Validity Range (DR)> ^ <Name Assembly Order(ID)> ^ <Effective Date (TS)> ^ <Expiration Date (TS)> ^ <Professional Suffix (ST)> ^ <Assigning Jurisdiction (CWE)> ^ < Assigning Agency or Department (CWE)>
-define(HL7_XCN, [
          {id , 'ST'},
          %% 2+3=nm TODO: what's this?
          {f_nm  , 'FN'}, %% typename FN does not exist ??? ^医師
          {g_nm  , 'ST'},
          {'2nd_nm'    , 'ST'},
          {suffix      , 'ST'},
          {prefix      , 'ST'},
          {degree      , 'IS'},
          {src_tbl     , 'IS'},
          {assign_authority , 'HD'},
          {nm_type_cd  , 'ID'},
          {id_chk_digit , 'ST'},
          {chk_digit_scheme , 'ID'},
          {id_type_cd  , 'ID'},
          {assign_facility , 'HD'},
          {nm_represent_cd , 'ID'},
          {nm_cntext   , 'CE'},
          {nm_valid_range  , 'DR'},
          {nm_assembly_order , 'ID'},
          {effective_date  , 'TS'},
          {expire_date , 'TS'},
          {pro_suffix  , 'ST'},
          {assign_jurisdict , 'CWE'},
          {assign_dpt  , 'CWE'}
         ]).

%% CX	チェックデジット付き拡張複合ID	<ID Number (ST)> ^ <Check Digit (ST)> ^ <Check Digit Scheme (ID)> ^ <Assigning Authority (HD)> ^ <Identifier Type Code (ID)> ^ <Assigning Facility (HD)> ^ <Effective Date (DT)> ^ <Expiration Date (DT)> ^ <AssigningJurisdiction (CWE)> ^ <Assigning Agency or Department (CWE)>
-define(HL7_CX, [
          {id         , 'ST'},
          {chk_digit	 , 'ST'},
          {chk_digit_scheme , 'ID'},
          {assign_authority , 'HD'},
          {id_type_cd , 'ID'},
          {assign_facility  , 'HD'},
          {effective_date   , 'DT'},
          {expire_date , 'DT'},
          {assign_jurisdict , 'CWE'},
          {assign_dpt , 'CWE'}
         ]).

%% XPN	拡張人名	<Family Name (FN)> ^ <Given Name (ST)> ^ <Second and Further Given Names orInitials Thereof (ST)> ^ <Suffix (e.g., JR or III) (ST)> ^ <Prefix (e.g., DR) (ST)> ^ <Degree(e.g., MD) (IS)> ^ <Name Type Code (ID)> ^ <Name Representation Code (ID)> ^ <NameContext (CWE)> ^ <Name Validity Range (DR)> ^ <Name Assembly Order (ID)> ^<Effective Date (TS)> ^ <Expiration Date (TS)> ^ <Professional Suffix (ST)>
-define(HL7_XPN, [
          {f_nm , 'ST'},
          {g_nm , 'ST'},
          {'2nd_nm' , 'ST'},
          {suffix , 'ST'},
          {prefix , 'ST'},
          {degree , 'IS'},
          {nm_type_cd , 'ID'},
          {nm_represent_cd , 'ID'},
          {nm_cntext  , 'CWE'},
          {nm_valid_range  , 'DR'},
          {nm_assembly_order , 'ID'},
          {effective_date , 'TS'},
          {expire_date , 'TS'},
          {pro_suffix  , 'ST'}
         ]).

%% XAD	拡張住所	<Street Address (SAD)> ^ <Other Designation (ST)> ^ <City (ST)> ^ <State or Province (ST)> ^ <Zip or Postal Code (ST)> ^ <Country (ID)> ^ <Address Type (ID)> ^ <Other eographic Designation (ST)> ^ <County/Parish Code (IS)> ^ <Census Tract (IS)> ^ <Address Representation Code (ID)> ^ <Address Validity Range (DR)> ^ <Effective Date(TS)> ^ <Expiration Date (TS)>
-define(HL7_XAD, [
          {street_address , 'SAD'}, %% TODO: not defined
          {other_design   , 'ST'},
          {city           , 'ST'},
          {state          , 'ST'},
          {zip_cd         , 'ST'},
          {country        , 'ID'},
          {address_type   , 'ID'},
          {other_geo_design , 'ST'},
          {parish_cd      , 'IS'},
          {census_tract   , 'IS'},
          {address_represent_cd , 'ID'},
          {address_valid_range  , 'DR'},
          {effective_date , 'TS'},
          {expire_date    , 'TS'}
         ]).

%% XTN	拡張テレコミュニケーション番号	<DEPRECATED-Telephone Number (ST)> ^ <Telecommunication Use Code (ID)> ^ <Telecommunication Equipment Type (ID)> ^ <Email Address (ST)> ^ <Country Code (NM)> ^ <Area/City Code (NM)> ^ <Local Number (NM)> ^ <Extension (NM)> ^ <Any Text (ST)> ^ <Extension Prefix (ST)> ^ <Speed Dial Code (ST)> ^ <Unformatted Telephone number (ST)>
-define(HL7_XTN, [
          {tel_number , 'ID'},
          {tel_use_cd , 'ID'},
          {tel_equip_type , 'ID'},
          {email_address	 , 'ST'},
          {country_cd , 'NM'},
          {city_cd    , 'NM'},
          {local_number , 'NM'},
          {ext        , 'NM'},
          {any_txt    , 'ST'},
          {ex_prefix  , 'ST'},
          {speed_dial_cd , 'ST'},
          {unform_tel_number , 'ST'}
         ]).

%% XON	拡張複合組織IDと名称	<Organization Name (ST)> ^ <Organization Name Type Code (IS)> ^ <ID Number (NM)> ^ <Check Digit (NM)> ^ <Check Digit Scheme (ID)> ^ <Assigning Authority (HD)> ^ <Identifier Type Code (ID)> ^ <Assigning Facility (HD)> ^ <Name epresentation Code(ID)> ^ <Organization Identifier (ST)>
-define(HL7_XON, [
          {organ_nm , 'ST'},
          {organ_nm_type_cd , 'IS'},
          {id       , 'NM'},
          {chk_digit , 'NM'},
          {chk_digit_scheme , 'ID'},
          {assign_authority , 'HD'},
          {id_type_cd , 'ID'},
          {assign_facility  , 'HD'},
          {nm_represent_cd  , 'ID'},
          {organ_id , 'ST'}
         ]).

%% PL	所在場所	<Point of Care (IS)> ^ <Room (IS)> ^ <Bed (IS)> ^ <Facility (HD)> ^ <LocationStatus (IS)> ^ <Person Location Type (IS)> ^ <Building (IS)> ^ <Floor (IS)> ^ <Location Description (ST)> ^ <Comprehensive Location dentifier(EI)> ^ <Assigning Authority for Location (HD)>
-define(HL7_PL, [
          {pnt_care , 'IS'},
          {room     , 'IS'},
          {bed      , 'IS'},
          {facility , 'HD'},
          {loc_status , 'IS'},
          {person_loc_type , 'IS'},
          {building , 'IS'},
          {floor    , 'IS'},
          {loc_description , 'ST'},
          {comp_loc_id , 'EI'},
          {loc_assign_authority , 'HD'}
         ]).

%% EI	エンティティ識別子	<Entity Identifier (ST)> ^ <Namespace ID (IS)> ^ <Universal ID (ST)> ^ <Universal ID Type (ID)>
-define(HL7_EI, [
          {entity_id , 'ST'},
          {namespace_id , 'IS'},
          {univ_id   , 'ST'},
          {univ_id_type , 'ID'}
         ]).


%% RPT	繰り返しパターン	<Repeat Pattern Code (CWE)> ^ <Calendar Alignment (ID)> ^ <Phase Range Begin Value (NM)> ^ <Phase Range End Value (NM)> ^ <Period Quantity (NM)> ^ <Period Units (IS)> ^ <Institution Specified Time (ID)> ^ <Event (ID)> ^ <Event Offset Quantity (NM)> ^ <Event Offset Units (IS)> ^ <General Timing Specification (GTS)>
-define(HL7_RPT, [
          {repete_pattern_cd   , 'CWE'},
          {calendar_alignment  , 'ID'},
          {phase_range_begin   , 'NM'},
          {phase_range_end     , 'NM'},
          {period_quant        , 'NM'},
          {period_units        , 'IS'},
          {institute_spec_time , 'ID'},
          {event               , 'ID'},
          {event_offset_quant  , 'NM'},
          {event_offset_units  , 'IS'},
          {general_tim_spec    , 'GTS'}
         ]).

%% CQ	単位付複合数量	<Quantity (NM)> ^ <Units (CWE)>
-define(HL7_CQ, [
          {quant_nm , 'NM'},
          {units    , 'CWE'}
         ]).

%% LA2	投薬場所	<Point of Care (IS)> ^ <Room (IS)> ^ <Bed (IS)> ^ <Facility (HD)> ^ <Location Status (IS)> ^ <Patient Location Type (IS)> ^ <Building (IS)> ^ <Floor (IS)> ^ <Street Address (ST)> ^ <Other Designation (ST)> ^ <City (ST)> ^ <State or Province (ST)> ^ <Zip or Postal Code (ST)> ^ <Country (ID)> ^ <Address Type (ID)> ^ <Other Geographic Designation (ST)>
-define(HL7_LA2, [
          {pnt_care , 'IS'},
          {room     , 'IS'},
          {bed      , 'IS'},
          {facility , 'HD'},
          {loc_status  , 'IS'},
          {pt_loc_type , 'IS'},
          {building , 'IS'},
          {floor    , 'IS'},
          {street_address , 'ST'},
          {other_design   , 'ST'},
          {city     , 'ST'},
          {state    , 'ST'},
          {zip_cd   , 'ST'},
          {country  , 'ID'},
          {address_type   , 'ID'},
          {other_geo_design , 'ST'}
         ]).

%% EIP	エンティティ識別子ペア	<Placer Assigned Identifier (EI)> ^ <Filler Assigned Identifier (EI)>
-define(HL7_EIP, [
          {placer_id , 'EI'},
          {filler_id , 'EI'}
         ]).

-type 'SI'() :: integer().

%% TODO: where job_class comes from?
%% JCC	職種コード／種類	<Job Category Code (ST)> ^ <Employment Status (ST)>
-define(HL7_JCC, [
          {job_cd     , 'ST'},
          {job_class  , 'ST'},
          {job_descript_txt , 'ST'}
         ]).

%% ZRD	放射線検査用薬剤やフィルムの情報（名称、コード、量）	<identifier (ST)> ^ <text (ST)> ^ <name of encoding system  (IS)> ^ <quantity (NM)> ^ <unit (CWE)> ^ <film partition number (NM)>
-define(HL7_ZRD, [
          {id    , 'ST'},
          {txt   , 'ST'},
          {nm_encode_sys , 'IS'},
          {quantity , 'NM'},
          {unit     , 'CWE'},
          {film_part_number , 'NM'}
         ]).

-define(HL7_PRIMITIVE_TYPES,
        [{'HD', ?HL7_HD}, {'CE', ?HL7_CE}, {'CNE', ?HL7_CNE}, {'CWE', ?HL7_CWE}, {'TS', ?HL7_TS},
         {'DR', ?HL7_DR}, {'MSG', ?HL7_MSG}, {'PT', ?HL7_PT}, {'VID', ?HL7_VID}, {'XCN', ?HL7_XCN},
         {'CX', ?HL7_CX}, {'XPN', ?HL7_XPN}, {'XAD', ?HL7_XAD}, {'XTN', ?HL7_XTN}, {'XON', ?HL7_XON},
         {'PL', ?HL7_PL}, {'EI', ?HL7_EI}, {'RPT', ?HL7_RPT}, {'CQ', ?HL7_CQ}, {'LA2', ?HL7_LA2},
         {'EIP', ?HL7_EIP}, {'JCC', ?HL7_JCC}, {'ZRD', ?HL7_ZRD}]).


%% １）青色は『JAHIS臨床検査データ交換規約Ver3.0の第５章関連情報詳細』に詳細な説明があります。
%% ２）赤色（LA2;RAX-11）は『JAHIS注射データ交換規約Ver1.0』のP138に詳細な説明があります。
%% ３）肌色（ZRD:ZE1-9）は『JAHIS放射線データ交換規約Ver2.2』のP38に詳細な説明があります。
%% ４）ZRDについては、『JAHIS内視鏡データ交換規約Ver1.0』のP40にもありますが、定義は３に内包されているので、３の定義でパースしてください。
