%%
%% Copyright (C) 2013-2013 Basho Technologies, Inc.
%%

-record(episode, {
          start_date,
          end_date,
          margin,
          admission,
          discharge,
          hl7_msgs = [],
          rezept_records = []
         }).

-define(BEGINNING_OF_THE_WORLD, <<"19000101">>).
-define(END_OF_THE_WORLD,       <<"30001231">>).
