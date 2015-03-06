-module(dpcs_io).

-export([put_record/5]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("riakc/include/riakc.hrl").

put_record(C , Mode, HospitalID, Date , {Key, CommonFields, RececdFields}) ->
    ContentType = "application/json",
    BucketName = Mode ++ ":" ++ HospitalID,
    Bucket = {<<"md">> , list_to_binary(BucketName)},
    JSONRecords = jsone:encode({CommonFields ++ RececdFields}, [native_utf8]),
    RiakObj = meddatum:maybe_new_ro(C, Bucket, list_to_binary(Key), JSONRecords, ContentType),
    riakc_pb_socket:put(C, RiakObj),
    ok.
