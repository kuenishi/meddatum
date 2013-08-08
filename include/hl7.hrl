

-record(hl7msg, {
          file :: filename:filename(),
          date :: string(),
          msg_type :: string(),
          msg_id :: string(),
          segments = []
         }).

-record('EVN', {%% EVN-0 segment ID
          segment_name = 'EVN',
          recored_date, %% EVN-2 date
          planned_date, %% EVN-3 planned date
          reason_code,  %% EVN-4 reason code
          operator_id,  %% EVN-5 operator id
          event_date,   %% EVN-6 the date exactly it happened
          fac           %% EVN-7 facility
         }).

-record('PID', {
          segment_name = 'PID',
          %% set_id,       %% PID-1
          patient_id,   %% PID-3  patient id
          name,         %% PID-5  patient name
          birth_age,    %% PID-7
          sex,          %% PID-8
          address,      %% PID-11
          housephone,   %% PID-13
          workphone,    %% PID-14
          last_updated  %% PID-33
         }).

%% P29 3.5.4
-record('NK1', {
          segment_name = 'NK1'
         }).

%% P30 3.5.5
-record('PV1', {
          segment_name = 'PV1',
          %% set_id,       %% PV1-1
          patient_type,    %% PV1-2
          current_place,   %% PV1-3
          %% hospitalized_reason, %% PV1-4
          previous_place,  %% PV1-5
          doctor,          %% PV1-6
          division,        %% PV1-10
          doctor_on_enter, %% PV1-17
          leave_reason,    %% PV1-36
          enter_date,      %% PV1-44
          leave_date       %% PV1-45
         }).

%% P33 3.5.6
-record('PV2', {
          segment_name = 'PV2'
         }).

%% P36 3.5.7
-record('DB1', {
          segment_name = 'DB1',
          set_id,      %% DB1-1
          handicap,    %% DB1-2
          handicapped  %% DB1-4
         }).

%% P37 3.5.8
-record('OBX', {
          segment_name = 'OBX',
          set_id,      %% OBX-1
          value_type,  %% OBX-2
          observation_id, %% Observation Identifier CWE 0 Yes 705
          %% Observation Sub-ID ST 0 No 20
          observation_value, %% Observation Value CD 0 Yes 581
          units, %% Units CWE 0 No 705
          %% References Range ST 0 No 60
          %% Abnormal Flags IS -1 No 5
          %% Probability NM 0 No 5
          %% Nature of Abnormal Test ID -1 No 2
          observation_result  %% Observation Result Status ID 0 Yes 1
         }).

%% P38 3.5.9
-record('AL1', {
          segment_name = 'AL1',
          set_id,         %% AL1-1: Set ID - AL1 (CE)
          allergen_type,  %% AL1-2: Allergen Type Code (CE) optional
          allergen_desc   %% AL1-3: Allergen Code/Mnemonic/Description (CE)
          %% AL1-4: Allergy Severity Code (CE) optional
          %% AL1-5: Allergy Reaction Code (ST) optional repeating
          %% AL1-6: Identification Date (DT) optional
         }).


%% P39 3.5.10
%% http://hl7api.sourceforge.net/v24/apidocs/ca/uhn/hl7v2/model/v24/segment/IN1.html
-record('IN1', {
          segment_name = 'IN1',
          set_id,         %% IN1-1: Set ID - IN1 (SI)
          insurance_plan_id, %% IN1-2: Insurance Plan ID (CE)
          insurance_company_id, %% IN1-3: Insurance Company ID (CX) repeating
          insurance_company,    %% IN1-4: Insurance Company Name (XON) optional repeating
          %% IN1-5: Insurance Company Address (XAD) optional repeating
          %% IN1-6: Insurance Co Contact Person (XPN) optional repeating
          %% IN1-7: Insurance Co Phone Number (XTN) optional repeating
          %% IN1-8: Group Number (ST) optional
          %% IN1-9: Group Name (XON) optional repeating
          insured_group_emp_id, %% IN1-10: Insured's Group Emp ID (CX) optional repeating
          insured_group_emp,    %% IN1-11: Insured's Group Emp Name (XON) optional repeating
          plan_effective_date,  %% IN1-12: Plan Effective Date (DT) optional
          plan_expiration_date, %% IN1-13: Plan Expiration Date (DT) optional
          %% IN1-14: Authorization Information (AUI) optional
          plan_type,            %% IN1-15: Plan Type (IS) optional
          %% IN1-16: Name Of Insured (XPN) optional repeating
          insured_relationship_to_patient  %% IN1-17: Insured's Relationship To Patient (CE) optional
          %% IN1-18: Insured's Date Of Birth (TS) optional
          %% IN1-19: Insured's Address (XAD) optional repeating
          %% IN1-20: Assignment Of Benefits (IS) optional
          %% IN1-21: Coordination Of Benefits (IS) optional
          %% IN1-22: Coord Of Ben. Priority (ST) optional
          %% IN1-23: Notice Of Admission Flag (ID) optional
          %% IN1-24: Notice Of Admission Date (DT) optional
          %% IN1-25: Report Of Eligibility Flag (ID) optional
          %% IN1-26: Report Of Eligibility Date (DT) optional
          %% IN1-27: Release Information Code (IS) optional
          %% IN1-28: Pre-Admit Cert (PAC) (ST) optional
          %% IN1-29: Verification Date/Time (TS) optional
          %% IN1-30: Verification By (XCN) optional repeating
          %% IN1-31: Type Of Agreement Code (IS) optional
          %% IN1-32: Billing Status (IS) optional
          %% IN1-33: Lifetime Reserve Days (NM) optional
          %% IN1-34: Delay Before L.R. Day (NM) optional
          %% IN1-35: Company Plan Code (IS) optional
          %% IN1-36: Policy Number (ST) optional
          %% IN1-37: Policy Deductible (CP) optional
          %% IN1-38: Policy Limit - Amount (CP) optional
          %% IN1-39: Policy Limit - Days (NM) optional
          %% IN1-40: Room Rate - Semi-Private (CP) optional
          %% IN1-41: Room Rate - Private (CP) optional
          %% IN1-42: Insured's Employment Status (CE) optional
          %% IN1-43: Insured's Administrative Sex (IS) optional
          %% IN1-44: Insured's Employer's Address (XAD) optional repeating
          %% IN1-45: Verification Status (ST) optional
          %% IN1-46: Prior Insurance Plan ID (IS) optional
          %% IN1-47: Coverage Type (IS) optional
          %% IN1-48: Handicap (IS) optional
          %% IN1-49: Insured's ID Number (CX) optional repeating
          %% IN1-50
          %% IN1-51
          %% IN1-52
          %% IN1-53
         }).

%% P42 3.6.5
-record('IAM', {
          segment_name = 'IAM',
          set_id, %% IAM-1: Set ID - IAM (SI)
          allergen_type, %% IAM-2: Allergen Type Code (CE) optional
          allergen_code, %% IAM-3: Allergen Code/Mnemonic/Description (CE)
          allergy_severity, %% IAM-4: Allergy Severity Code (CE) optional
          allergy_reaction, %% IAM-5: Allergy Reaction Code (ST) optional repeating
          allergy_action,   %% IAM-6: Allergy Action Code (CNE)
          allergy_id,       %% IAM-7: Allergy Unique Identifier (EI)
          action_reason,    %% IAM-8: Action Reason (ST) optional
          sensitivity,      %% IAM-9: Sensitivity to Causative Agent Code (CE) optional
          allergen_group,   %% IAM-10: Allergen Group Code/Mnemonic/Description (CE) optional
          onset_date,       %% IAM-11: Onset Date (DT) optional
          onset_date_text,  %% IAM-12: Onset Date Text (ST) optional
          reported_date,    %% IAM-13: Reported Date/Time (TS) optional
          reported_by,      %% IAM-14: Reported By (XPN) optional
          reporter_relation,%% IAM-15: Relationship to Patient Code (CE) optional
          %% IAM-16: Alert Device Code (CE) optional
          allergy_clinical_status, %% IAM-17: Allergy Clinical Status Code (CE) optional
          statused_by_person,      %% IAM-18: Statused by Person (XCN) optional
          statused_by_org,         %% IAM-19: Statused by Organization (XON) optional
          statused_date            %% IAM-20: Statused at Date/Time (TS) optional
         }).

%% P45 3.7.3
%% http://www.mexi.be/documents/hl7/ch120022.htm
-record('PRB', {
          segment_name = 'PRB',
          action_code,   %% PRB-1
          action_date,   %% PRB-2
          problem_id,    %% PRB-3
          problem_instance_id,  %% PRB-4
          episode_care_id,   %% PRB-5
          master_problem_list_num,      %% PRB6
          problem_established_date,  %% PRB-7
          problem_resolution_expected_date, %% PRB-8
          problem_resolution_date, %% PRB-9
          problem_classification,      %% PRB-10
          problem_management_discipline,
          problem_persistence,
          problem_confirmation_status,
          problem_lifecycle_status,
          problem_lifecycle_status_date,
          problem_onset_date,
          problem_onset_text,
          problem_ranking,
          problem_certainty,
          problem_probability,
          problem_awareness,
          problem_prognosis,
          prognosis_awareness,
          family_prognosis_awareness,
          security_sensitivity
         }).

%% P47 3.7.4
-record('ZPR', {
          segment_name = 'ZPR',
          prefix_id,   %% ZPR-1
          disease_id,  %% ZPR-2
          suffix_id,   %% ZPR-3
          prefix_code, %% ZPR-4
          disease_code,%% ZPR-5
          suffix_code, %% ZPR-6
          commentary   %% ZPR-7
         }).

%% P48 3.7.6
-record('ORC', {
          segment_name = 'ORC',
          order_control,    %% ORC-1
          placer_order_num, %% ORC-2
          filler_order_num, %% ORC-3
          placer_group_num, %% ORC-4
          transaction_date, %% ORC-9
          entered_by,       %% ORC-10
          ordering_provider,%% ORC-12
          enterer_location, %% ORC-13
          order_effective_date, %% ORC-15
          reason_code,      %% ORC-16
          entering_org,     %% ORC-17
          entering_device,  %% ORC-18
          ordering_fac,     %% ORC-21
          ordering_fac_address, %% ORC-22
          ordering_fac_phone, %% ORC-23
          order_type        %% ORC-29
         }).

%% P53 3.8.5
%%-record('ORC', {}).

%% P55 3.8.6
-record('TQ1', {
          segment_name = 'TQ1',
          set_id,           %% TQ1-1
          repeat,           %% TQ1-3
          start_date,       %% TQ1-7
          priority,         %% TQ1-9
          conjunction       %% TQ1-12
         }).

%% P56 3.8.7
-record('ODS', {
          segment_name = 'ODS'
         }).

%% P59 3.9.5
%% -record('ORC', {}).

%% P62 3.9.6
-record('RXE', {
          segment_name = 'RXE',
          give_code,   %% RXE-2
          give_amount_min, %% RXE-3
          give_amount_max, %% RXE-4
          give_units,      %% RXE-5
          dosage_form,     %% RXE-6
          providers_instruction, %% RXE-7
          dispence_amount, %% RXE-10
          dispence_units,  %% RXE-11
          doctor_dea,      %% RXE-13
          pharmacist_id,   %% RXE-14
          prescription_num,%% RXE-15
          refills_remain,  %% RXE-16
          doses_dispensed, %% RXE-17
          recent_refill_date, %% RXE-18
          daily_dose,      %% RXE-19
          dispense_instructions, %% RXE-21
          give_per,        %% RXE-22
          give_factor,     %% RXE-25
          give_factor_units,%% RXE-26
          give_instructions,%% RXE-27
          dispence_package_size, %% RXE-28
          dispence_package_units, %% RXE-29
          dispence_package_method, %% RXE-30
          original_order_date, %% RXE-32
          give_factor_quant,   %% RXE-33
          give_factor_quant_units,   %% RXE-34
          control_schedule,    %% RXE-35
          dispance_status,     %% RXE-36
          medicine_replacement,%% RXE-37
          first_refill_dept,   %% RXE-38
          first_refill_quant,  %% RXE-39
          refill_dept,         %% RXE-40
          refill_dept_addr,    %% RXE-41
          deliver_loc,         %% RXE-42
          deliver_address,     %% RXE-43
          order_type           %% RXE-44
         }).

%% P67 3.9.7 -> TQ1

%% P67 3.9.8
-record('RXR', {
          segment_name = 'RXR',
          route,   %% RXR-1
          site,    %% RXR-2
          device,  %% RXR-3
          method   %% RXR-4
         }).

%% P74 3.11.5 ORC

%% P82 3.11.9
%% http://hl7api.sourceforge.net/v24/apidocs/ca/uhn/hl7v2/model/v24/segment/RXC.html
-record('RXC', {
          segment_name = 'RXC',
          component_type,  %% RXC-1: RX Component Type (ID)
          component_code,  %% RXC-2: Component Code (CE)
          component_amount,%% RXC-3: Component Amount (NM)
          component_units, %% RXC-4: Component Units (CE)
          component_strength, %% RXC-5: Component Strength (NM) optional
          component_strength_units, %% RXC-6: Component Strength Units (CE) optional
          supplementary_code, %% RXC-7: Supplementary Code (CE) optional repeating
          component_strength_quant, %% RXC-8
          component_strength_quant_units %% RXC-9
         }).

%% P92 3.13.5
-record('SPM', {
          segment_name = 'SPM',
          set_id,         %% SPM-1
          specimen_id,    %% SPM-2
          specimen_type,  %% SPM-4
          specimen_origin,%% SPM-8
          date            %% SPM-17
         }).

%% P96 3.13.7
-record('OBR', {
          segment_name = 'OBR',
          set_id,           %% OBR-1
          placer_order_num, %% OBR-2
          filler_order_num, %% OBR-3
          service_id,       %% OBR-4
          observation_date, %% OBR-7
          observation_end_date, %% OBR-8
          danger_code,      %% OBR-12
          ordering_provider,%% OBR-16
          change_date       %% OBR-22
         }).

%% %% P
%% -record('', {}).
%% %% P
%% -record('', {}).
%% %% P
%% -record('', {}).
