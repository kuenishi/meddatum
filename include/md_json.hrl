

-define(JSON_RECORD_OPENER(RecordName0__),
        fun(Record__) when is_record(Record__, RecordName0__) ->
                Fields__ = [atom_to_binary(Field__, utf8) ||
                             Field__ <- record_info(fields, RecordName0__)],
                Values__ = tl(tuple_to_list(Record__)),
                [{F__,V__} || {F__,V__} <- lists:zip(Fields__, Values__),
                          V__ =/= undefined, V__ =/= null]
        end).

%% RecordName should be atom at compile time
-define(JSON_RECORD_ENCODER(RecordName__),
        fun(Record__) when is_record(Record__, RecordName__) ->
                Fields__ = [atom_to_binary(Field__, utf8) ||
                             Field__ <- record_info(fields, RecordName__)],
                Values__ = tl(tuple_to_list(Record__)),
                List__ = [{F__,V__} ||
                             {F__,V__} <- lists:zip(Fields__, Values__),
                             V__ =/= undefined, V__ =/= null],
                jsone:encode({List__}, [native_utf8])
        end).

-define(JSON_RECORD_DECODER(RecordName__),
        fun(Bin__) ->
                Fields__ = [atom_to_binary(Field__, utf8) ||
                             Field__ <- record_info(fields, RecordName__)],
                {JSON__} = jsone:decode(Bin__),
                Values__ = [ proplists:get_value(FieldName__, JSON__) ||
                             FieldName__ <- Fields__ ],
                list_to_tuple([RecordName__|Values__])
        end).
