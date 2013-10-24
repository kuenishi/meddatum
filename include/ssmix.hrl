-record(ssmix_msg, {
          hospital_id :: string(), %% hospital id
          patient_id :: string(), %% patient id
          date :: string(), %% YYYYMMDD
          datatype :: string(), %% OML-11 or else
          path :: filename:filename() %% path/to/the/file
         }).

%% From SS-MIX2 標準化ストレージ　構成の説明と構築ガイドライン
%% Directory name (SS-MIX data type) to HL7 message type
%%  1 ADT-00    ADT^A08
%%  2 ADT-00    ADT^A23
%%  3 ADT-01    ADT^A54
%%  4 ADT-01    ADT^A55
%%  5 ADT-12    ADT^A04
%%  6 ADT-21    ADT^A14
%%  7 ADT-21    ADT^A27
%%  8 ADT-22    ADT^A01
%%  9 ADT-22    ADT^A11
%% 10 ADT-31    ADT^A21
%% 11 ADT-31    ADT^A52
%% 12 ADT-32    ADT^A22
%% 13 ADT-32    ADT^A53
%% 14 ADT-41 () ADT^A15
%% 15 ADT-41 () ADT^A26
%% 16 ADT-42 () ADT^A02
%% 17 ADT-42 () ADT^A12
%% 18 ADT-51    ADT^A16
%% 19 ADT-51    ADT^A25
%% 20 ADT-52    ADT^A03
%% 21 ADT-52    ADT^A13
%% 22 ADT-61    ADT^A60 
%% 23 PPR-01    PPR^ZD1 
%% 24 OMD       OMD^O03
%% 25 OMP-01    RDE^11  
%% 26 OMP-11    RAS^O17 
%% 27 OMP-02    RDE^11  
%% 28 OMP-12    RAS^O17 
%% 29 OML-01    OML^O33
%% 30 OML-11    OUL^R22 
%% 31 OMG-01    OMG^O19
%% 32 OMG-11    OMI^Z23 
%% 33 OMG-02    OMG^O19 
%% 34 OMG-12    OMI^Z23 
%% 35 OMG-03    OMG^O19 
%% 36 OMG-13    ORU^R01 
