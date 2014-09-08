
%% RecordName should be atom at compile time
-define(JSON_RECORD_ENCODER(RecordName),
        fun(Record) when is_record(Record, RecordName) ->
                Fields = [atom_to_binary(Field, utf8) ||
                             Field <- record_info(fields, RecordName)],
                Values = tl(tuple_to_list(Record)),
                List = [{F,V} || {F,V} <- lists:zip(Fields, Values),
                                 V =/= undefined, V =/= null],
                jsone:encode({List}, [native_utf8])
        end).

-define(JSON_RECORD_DECODER(RecordName),
        fun(Bin) ->
                Fields = [atom_to_binary(Field, utf8) ||
                             Field <- record_info(fields, RecordName)],
                {JSON} = jsone:decode(Bin),
                Values = [ proplists:get_value(FieldName, JSON) ||
                             FieldName <- Fields ],
                list_to_tuple([RecordName|Values])
        end).
