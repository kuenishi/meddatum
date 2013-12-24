-include("rezept_codes.hrl").

-type string_date() :: string().

-record(recept, {
          date          :: binary(),
          patient_id    :: binary(),
          hospital_id   :: binary(),
          segments = [] :: list(),
          file          :: binary(), %% <- filename:filename()
          checksum      :: binary()
         }).

%% TYPE_CONVERSION:
%%   {integer => "数字",
%%    latin1  => "英数",
%%    unicode => "漢字",
%%    date    => "数字"} %% GMMYY
%%   %% or {maybe, Type}

-include("rezept_common_records.hrl").
-include("rezept_med.hrl").
-include("rezept_dpc.hrl").
