%%% @copyright (C) 2013, Basho Technologies Inc.,
%%% @doc
%%%  HL7 importer, transformer.
%%% @end
%%% Created : 17 Jul 2013 by UENISHI Kota <kota@basho.com>

-module(hl7).

-export([parse/2, to_json/1]).

-include_lib("eunit/include/eunit.hrl").
-include("hl7.hrl").

-spec parse(filename:filename(), file:file_info()) -> ok | {error, any()}.
parse(Filename, _Info)->
    parse_msg(Filename).

parse_msg(File)->
    %%?debugVal(File),
    case read_all_lines(File) of %% I NEED SIMPLE MAYBE MONAD
        {ok, Lines0} ->
            case parse_0(Lines0) of
                {ok, Msg} ->
                    parse_1(Msg, tl(Lines0));
                {error, _} = E ->
                    %% TODO: output log here
                    E
            end;
        {error, _} = E ->
            %% TODO: output log here
            E
    end.

read_all_lines(File) ->
    Command = "nkf -w " ++ File,
    Port = open_port({spawn, Command}, [stream,in,binary,eof]),
    {ok, Lines} = get_all_lines(Port, <<>>),
    port_close(Port),
    {ok, Lines}.

get_all_lines(Port, Binary) ->
    receive
        {Port, {data, Data}} ->
            get_all_lines(Port, <<Binary/binary, Data/binary>>);
        {Port, eof} ->
            FileContent = unicode:characters_to_list(Binary),
            Lines = string:tokens(FileContent, "\r"),
            {ok, Lines};
        Other -> ?debugVal(Other)
    end.
             

parse_0([Line|_Lines]) ->
    %% lists:foreach(fun(L)->pp(L)end, Lines),
    %% case binary:split(Line, <<"|">>, [global]) of
    %% case string:tokens(Line, "|") of
    Tokens = re:split(Line, "[|]", [{return,list},unicode]),
    case Tokens of
        ["MSH"|_] = Tokens ->
            Msg = 'MSH'(Tokens),
            {ok, Msg};
        _O ->
            erlang:display(_O),
            {error, bad_format}
    end.


%%     [ 日本語どうでしたっけ？いける！！
parse_1(#hl7msg{segments=Segs} = Msg, []) ->
    {ok, Msg#hl7msg{segments=lists:reverse(Segs)}};

parse_1(Msg, [Line|Lines] = _Lines) ->
    Tokens = re:split(Line, "[|]", [{return,list},unicode]),
    
    case hd(Tokens) of
        
        Segment when is_list(Segment) ->
            {ok, NewMsg} = handle_segment(Segment, Tokens, Msg),
            parse_1(NewMsg, Lines);

        _Other ->
            io:format("unknown segment: ~s", [_Other]),
            {error, badarg}
    end.

%% charcode is defined as ISO IR87 / JIS Kanji code.

%% From page 24 of SS-MIX2 標準化ストレージ仕様書
'MSH'(Tokens) ->
    ["MSH", _Seperators, _SenderApp, _Sender, _ReceiverApp, _Receiver,
     _Date, _Security, _MsgType, _MsgID, _ProcessID,
     "2.5",  %% Version ID is fixed
     _SeqNum, _ContPointer, _YN, _, _CountryCode,
     "~ISO IR87", %% Charcode is fixed (but already translated to UTF8 :)
     _Lang,
     "ISO 2022-1994"|_] = Tokens,
    #hl7msg{date = maybe_nth(7, Tokens),
            msg_type = maybe_nth(9, Tokens),
            msg_id = maybe_nth(10, Tokens)}.

handle_segment("PID", Tokens, Msg) ->     
    Pid = #'PID'{
             patient_id = maybe_nth(4, Tokens),
             name =       maybe_nth(6, Tokens),
             birth_age =  maybe_nth(8, Tokens),
             sex = maybe_nth(9, Tokens),
             address = maybe_nth(12, Tokens),
             housephone = maybe_nth(14, Tokens),
             workphone = maybe_nth(15, Tokens),
             last_updated = maybe_nth(34, Tokens)
            },
    {ok, append_segment(Msg, Pid)};

handle_segment("EVN", Tokens, Msg) ->
    Evn = #'EVN'{
             recored_date = maybe_nth(3, Tokens),
             planned_date = maybe_nth(4, Tokens),
             reason_code = maybe_nth(5, Tokens),
             operator_id = maybe_nth(6, Tokens),
             event_date = maybe_nth(7, Tokens),
             fac = maybe_nth(8, Tokens)
            },
    {ok, append_segment(Msg, Evn)};

handle_segment("PV1", Tokens, Msg) ->
    Pv1 = #'PV1'{
             patient_type = maybe_nth(3, Tokens),
             current_place = maybe_nth(4, Tokens),
             previous_place= maybe_nth(7, Tokens),
             doctor = maybe_nth(8, Tokens),
             division = maybe_nth(11, Tokens),
             doctor_on_enter = maybe_nth(18, Tokens),
             leave_reason = maybe_nth(37, Tokens),
             enter_date = maybe_nth(45, Tokens),
             leave_date = maybe_nth(46, Tokens)
            },
    {ok, append_segment(Msg, Pv1)};

handle_segment("SPM", Tokens, Msg) ->
    SPM = #'SPM'{
             set_id = maybe_nth(2, Tokens),
             specimen_id = maybe_nth(3, Tokens),
             specimen_type = maybe_nth(5, Tokens),
             specimen_origin = maybe_nth(9, Tokens),
             date = maybe_nth(18, Tokens)
            },
    {ok, append_segment(Msg, SPM)};

handle_segment("OBR", Tokens, Msg) ->
    OBR = #'OBR'{
             set_id = maybe_nth(2, Tokens),
             placer_order_num = maybe_nth(3, Tokens), %% OBR-2
             filler_order_num = maybe_nth(4, Tokens), %% OBR-3
             service_id = maybe_nth(5, Tokens),       %% OBR-4
             observation_date = maybe_nth(8, Tokens), %% OBR-7
             observation_end_date = maybe_nth(9, Tokens), %% OBR-8
             danger_code = maybe_nth(13, Tokens),      %% OBR-12
             ordering_provider = maybe_nth(17, Tokens),%% OBR-16
             change_date = maybe_nth(23, Tokens)      %% OBR-22
            },
    {ok, append_segment(Msg, OBR)};

handle_segment("ORC", Tokens, Msg) ->
    ORC = #'ORC'{
             order_control = maybe_nth(2,Tokens),    %% ORC-1
             placer_order_num = maybe_nth(3,Tokens), %% ORC-2
             filler_order_num = maybe_nth(4,Tokens), %% ORC-3
             placer_group_num = maybe_nth(5,Tokens), %% ORC-4
             transaction_date = maybe_nth(10,Tokens), %% ORC-9
             entered_by = maybe_nth(11,Tokens),       %% ORC-10
             ordering_provider = maybe_nth(13,Tokens),%% ORC-12
             enterer_location = maybe_nth(14,Tokens), %% ORC-13
             order_effective_date = maybe_nth(16,Tokens), %% ORC-15
             reason_code = maybe_nth(17,Tokens),      %% ORC-16
             entering_org = maybe_nth(18,Tokens),     %% ORC-17
             entering_device = maybe_nth(19,Tokens),  %% ORC-18
             ordering_fac = maybe_nth(22,Tokens),     %% ORC-21
             ordering_fac_address = maybe_nth(23,Tokens), %% ORC-22
             ordering_fac_phone = maybe_nth(24,Tokens), %% ORC-23
             order_type = maybe_nth(30,Tokens)        %% ORC-29
            },
    {ok, append_segment(Msg, ORC)};

handle_segment("OBX", Tokens, Msg) ->
    OBX = #'OBX'{
             set_id = maybe_nth(2,Tokens),      %% OBX-1
             value_type = maybe_nth(3,Tokens),  %% OBX-2
             observation_id = maybe_nth(4,Tokens), %% Observation Identifier CWE 0 Yes 705
             %% Observation Sub-ID ST 0 No 20
             observation_value = maybe_nth(6,Tokens), %% Observation Value CD 0 Yes 581
             units = maybe_nth(7,Tokens), %% Units CWE 0 No 705
             %% References Range ST 0 No 60
             %% Abnormal Flags IS -1 No 5
             %% Probability NM 0 No 5
             %% Nature of Abnormal Test ID -1 No 2
             observation_result = maybe_nth(11, Tokens) %% Observation Result Status ID 0 Yes 1
            },
    {ok, append_segment(Msg, OBX)};

handle_segment("TQ1", Tokens, Msg) ->
    TQ1 = #'TQ1'{
             set_id = maybe_nth(2,Tokens),           %% TQ1-1
             repeat = maybe_nth(4,Tokens),           %% TQ1-3
             start_date = maybe_nth(8,Tokens),       %% TQ1-7
             priority = maybe_nth(10,Tokens),         %% TQ1-9
             conjunction = maybe_nth(13,Tokens)      %% TQ1-12
            },
    {ok, append_segment(Msg, TQ1)};

handle_segment("RXE", Tokens, Msg) ->
%% P62 3.9.6
    RXE = #'RXE'{
             give_code = maybe_nth(3,Tokens),   %% RXE-2
             give_amount_min = maybe_nth(4,Tokens), %% RXE-3
             give_amount_max = maybe_nth(5,Tokens), %% RXE-4
             give_units = maybe_nth(6,Tokens),      %% RXE-5
             dosage_form = maybe_nth(7,Tokens),     %% RXE-6
             providers_instruction = maybe_nth(8,Tokens), %% RXE-7
             dispence_amount = maybe_nth(11,Tokens), %% RXE-10
             dispence_units = maybe_nth(12,Tokens),  %% RXE-11
             doctor_dea = maybe_nth(14,Tokens),      %% RXE-13
             pharmacist_id = maybe_nth(15,Tokens),   %% RXE-14
             prescription_num = maybe_nth(16,Tokens),%% RXE-15
             refills_remain = maybe_nth(17,Tokens),  %% RXE-16
             doses_dispensed = maybe_nth(18,Tokens), %% RXE-17
             recent_refill_date = maybe_nth(19,Tokens), %% RXE-18
             daily_dose = maybe_nth(20,Tokens),      %% RXE-19
             dispense_instructions = maybe_nth(22,Tokens), %% RXE-21
             give_per = maybe_nth(23,Tokens),        %% RXE-22
             give_factor = maybe_nth(26,Tokens),     %% RXE-25
             give_factor_units = maybe_nth(27,Tokens),%% RXE-26
             give_instructions = maybe_nth(28,Tokens),%% RXE-27
             dispence_package_size = maybe_nth(29,Tokens), %% RXE-28
             dispence_package_units = maybe_nth(30,Tokens), %% RXE-29
             dispence_package_method = maybe_nth(31,Tokens), %% RXE-30
             original_order_date = maybe_nth(33,Tokens), %% RXE-32
             give_factor_quant = maybe_nth(34,Tokens),   %% RXE-33
             give_factor_quant_units = maybe_nth(35,Tokens),   %% RXE-34
             control_schedule = maybe_nth(36,Tokens),    %% RXE-35
             dispance_status = maybe_nth(37,Tokens),     %% RXE-36
             medicine_replacement = maybe_nth(38,Tokens),%% RXE-37
             first_refill_dept = maybe_nth(39,Tokens),   %% RXE-38
             first_refill_quant = maybe_nth(40,Tokens),  %% RXE-39
             refill_dept = maybe_nth(41,Tokens),         %% RXE-40
             refill_dept_addr = maybe_nth(42,Tokens),    %% RXE-41
             deliver_loc = maybe_nth(43,Tokens),         %% RXE-42
             deliver_address = maybe_nth(44,Tokens),     %% RXE-43
             order_type = maybe_nth(45,Tokens)          %% RXE-44
            },
    {ok, append_segment(Msg, RXE)};

handle_segment("RXR", Tokens, Msg) ->
   RXR = #'RXR'{
            route = maybe_nth(2, Tokens),   %% RXR-1
            site = maybe_nth(3, Tokens),    %% RXR-2
            device = maybe_nth(4, Tokens),  %% RXR-3
            method = maybe_nth(5, Tokens)   %% RXR-4
           },
    {ok, append_segment(Msg, RXR)};

handle_segment("PRB", Tokens, Msg) ->
    PRB = #'PRB'{
             action_code = maybe_nth(2,Tokens),   %% PRB-1
             action_date = maybe_nth(3,Tokens),   %% PRB-2
             problem_id = maybe_nth(4,Tokens),    %% PRB-3
             problem_instance_id = maybe_nth(5,Tokens),  %% PRB-4
             episode_care_id = maybe_nth(6,Tokens),   %% PRB-5
             master_problem_list_num = maybe_nth(7,Tokens),      %% PRB6
             problem_established_date = maybe_nth(8,Tokens),  %% PRB-7
             problem_resolution_expected_date = maybe_nth(9,Tokens), %% PRB-8
             problem_resolution_date = maybe_nth(10,Tokens), %% PRB-9
             problem_classification = maybe_nth(11,Tokens),      %% PRB-10
             problem_management_discipline = maybe_nth(12,Tokens),
             problem_persistence = maybe_nth(13,Tokens),
             problem_confirmation_status = maybe_nth(14,Tokens),
             problem_lifecycle_status = maybe_nth(15,Tokens),
             problem_lifecycle_status_date = maybe_nth(16,Tokens),
             problem_onset_date = maybe_nth(17,Tokens),
             problem_onset_text = maybe_nth(18,Tokens),
             problem_ranking = maybe_nth(19,Tokens),
             problem_certainty = maybe_nth(20,Tokens),
             problem_probability = maybe_nth(21,Tokens),
             problem_awareness = maybe_nth(22,Tokens),
             problem_prognosis = maybe_nth(23,Tokens),
             prognosis_awareness = maybe_nth(24,Tokens),
             family_prognosis_awareness = maybe_nth(25,Tokens),
             security_sensitivity = maybe_nth(26,Tokens)
            },
    {ok, append_segment(Msg, PRB)};

handle_segment("ZPR", Tokens, Msg) ->
    ZPR = #'ZPR'{
             prefix_id = maybe_nth(2,Tokens),   %% ZPR-1
             disease_id = maybe_nth(3,Tokens),  %% ZPR-2
             suffix_id = maybe_nth(4,Tokens),   %% ZPR-3
             prefix_code = maybe_nth(5,Tokens), %% ZPR-4
             disease_code = maybe_nth(6,Tokens),%% ZPR-5
             suffix_code = maybe_nth(7,Tokens), %% ZPR-6
             commentary = maybe_nth(8, Tokens)   %% ZPR-7
            },
    {ok, append_segment(Msg, ZPR)};

handle_segment("IAM", Tokens, Msg) ->
    IAM = #'IAM'{
          set_id = maybe_nth(2,Tokens), %% IAM-1: Set ID - IAM (SI)
          allergen_type = maybe_nth(3,Tokens), %% IAM-2: Allergen Type Code (CE) optional
          allergen_code = maybe_nth(4,Tokens), %% IAM-3: Allergen Code/Mnemonic/Description (CE)
          allergy_severity = maybe_nth(5,Tokens), %% IAM-4: Allergy Severity Code (CE) optional
          allergy_reaction = maybe_nth(6,Tokens), %% IAM-5: Allergy Reaction Code (ST) optional repeating
          allergy_action = maybe_nth(7,Tokens),   %% IAM-6: Allergy Action Code (CNE)
          allergy_id = maybe_nth(8,Tokens),       %% IAM-7: Allergy Unique Identifier (EI)
          action_reason = maybe_nth(9,Tokens),    %% IAM-8: Action Reason (ST) optional
          sensitivity = maybe_nth(10,Tokens),      %% IAM-9: Sensitivity to Causative Agent Code (CE) optional
          allergen_group = maybe_nth(11,Tokens),   %% IAM-10: Allergen Group Code/Mnemonic/Description (CE) optional
          onset_date = maybe_nth(12,Tokens),       %% IAM-11: Onset Date (DT) optional
          onset_date_text = maybe_nth(13,Tokens),  %% IAM-12: Onset Date Text (ST) optional
          reported_date = maybe_nth(14,Tokens),    %% IAM-13: Reported Date/Time (TS) optional
          reported_by = maybe_nth(15,Tokens),      %% IAM-14: Reported By (XPN) optional
          reporter_relation = maybe_nth(16,Tokens),%% IAM-15: Relationship to Patient Code (CE) optional
          %% IAM-16: Alert Device Code (CE) optional
          allergy_clinical_status = maybe_nth(18,Tokens), %% IAM-17: Allergy Clinical Status Code (CE) optional
          statused_by_person = maybe_nth(19,Tokens),      %% IAM-18: Statused by Person (XCN) optional
          statused_by_org = maybe_nth(20,Tokens),         %% IAM-19: Statused by Organization (XON) optional
          statused_date = maybe_nth(21, Tokens)            %% IAM-20: Statused at Date/Time (TS) optional
         },
    {ok, append_segment(Msg, IAM)};

handle_segment("DB1", Tokens, Msg) ->
    DB1 = #'DB1'{
             set_id = maybe_nth(2, Tokens),      %% DB1-1
             handicap = maybe_nth(3, Tokens),    %% DB1-2
             handicapped = maybe_nth(4, Tokens)  %% DB1-4
            },
    {ok, append_segment(Msg, DB1)};

handle_segment("IN1", Tokens, Msg) ->
    IN1 = #'IN1'{
             set_id = maybe_nth(2,Tokens),         %% IN1-1: Set ID - IN1 (SI)
             insurance_plan_id = maybe_nth(3,Tokens), %% IN1-2: Insurance Plan ID (CE)
             insurance_company_id = maybe_nth(4,Tokens), %% IN1-3: Insurance Company ID (CX) repeating
             insurance_company = maybe_nth(5,Tokens),    %% IN1-4: Insurance Company Name (XON) optional repeating
             insured_group_emp_id = maybe_nth(11,Tokens), %% IN1-10: Insured's Group Emp ID (CX) optional repeating
             insured_group_emp = maybe_nth(12,Tokens),    %% IN1-11: Insured's Group Emp Name (XON) optional repeating
             plan_effective_date = maybe_nth(13,Tokens),  %% IN1-12: Plan Effective Date (DT) optional
             plan_expiration_date = maybe_nth(14,Tokens), %% IN1-13: Plan Expiration Date (DT) optional
             plan_type = maybe_nth(16,Tokens),            %% IN1-15: Plan Type (IS) optional
             insured_relationship_to_patient = maybe_nth(18,Tokens)  %% IN1-17: Insured's Relationship To Patie
            },
    {ok, append_segment(Msg, IN1)};

handle_segment("AL1", Tokens, Msg) ->
    AL1 = #'AL1'{
             set_id = maybe_nth(2, Tokens),         %% AL1-1: Set ID - AL1 (CE)
             allergen_type = maybe_nth(3, Tokens),  %% AL1-2: Allergen Type Code (CE) optional
             allergen_desc = maybe_nth(4, Tokens)   %% AL1-3: Allergen Code/Mnemonic/Description (CE)
            },
    {ok, append_segment(Msg, AL1)};

handle_segment("RXC", Tokens, Msg) ->
    RXC = #'RXC'{
             component_type = maybe_nth(2,Tokens),  %% RXC-1: RX Component Type (ID)
             component_code = maybe_nth(3,Tokens),  %% RXC-2: Component Code (CE)
             component_amount = maybe_nth(4,Tokens),%% RXC-3: Component Amount (NM)
             component_units = maybe_nth(5,Tokens), %% RXC-4: Component Units (CE)
             component_strength = maybe_nth(6,Tokens), %% RXC-5: Component Strength (NM) optional
             component_strength_units = maybe_nth(7,Tokens), %% RXC-6: Component Strength Units (CE) optional
             supplementary_code = maybe_nth(8,Tokens), %% RXC-7: Supplementary Code (CE) optional repeating
             component_strength_quant = maybe_nth(9,Tokens), %% RXC-8
             component_strength_quant_units = maybe_nth(10,Tokens) %% RXC-9
            },
    {ok, append_segment(Msg, RXC)};

handle_segment([16#1C], [[16#1C]], #hl7msg{segments=_Segs} = Msg) ->
    {ok, Msg}.
    %%{ok, Msg#hl7msg{segments=lists:reverse(Segs)}}.

    
append_segment(#hl7msg{segments = Segs} = Msg, Seg) ->
    Msg#hl7msg{segments = [Seg|Segs]}.

maybe_nth(N, List) ->
    try
        case lists:nth(N, List) of
            "" -> null;
            Term -> unicode:characters_to_binary(Term)
        end
    catch _:_ ->
            null
    end.

-define(DECLEARE_TO_JSON(Segment),
        {Segment, record_info(fields, Segment)}
       ).

encoder() ->
    jsonx:encoder([{hl7msg, record_info(fields, hl7msg)},
                   ?DECLEARE_TO_JSON('EVN'),
                   ?DECLEARE_TO_JSON('PID'),
                   ?DECLEARE_TO_JSON('NK1'),
                   ?DECLEARE_TO_JSON('PV1'),
                   ?DECLEARE_TO_JSON('PV2'),
                   ?DECLEARE_TO_JSON('DB1'),
                   ?DECLEARE_TO_JSON('OBX'),
                   ?DECLEARE_TO_JSON('AL1'),
                   ?DECLEARE_TO_JSON('IN1'),
                   ?DECLEARE_TO_JSON('IAM'),
                   ?DECLEARE_TO_JSON('PRB'),
                   ?DECLEARE_TO_JSON('ZPR'),
                   ?DECLEARE_TO_JSON('ORC'),
                   ?DECLEARE_TO_JSON('TQ1'),
                   ?DECLEARE_TO_JSON('ODS'),
                   ?DECLEARE_TO_JSON('RXE'),
                   ?DECLEARE_TO_JSON('RXR'),
                   ?DECLEARE_TO_JSON('RXC'),
                   ?DECLEARE_TO_JSON('SPM'),
                   ?DECLEARE_TO_JSON('OBR')],
                  [{ignore, [null]}]).

to_json(#hl7msg{} = HL7Msg) ->
    E = encoder(),
    E(HL7Msg).
                
