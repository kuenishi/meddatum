-module(dpcs_io).

-include("meddatum.hrl").
-include_lib("riakc/include/riakc.hrl").

-export([put_record/5]).

put_record(C, Mode, HospitalID, _Date, {Key, CommonFields, RececdFields}) ->
    BucketName = Mode ++ ":" ++ HospitalID,
    Bucket = meddatum:true_bucket_name(list_to_binary(BucketName)),
    JSONRecords = jsone:encode({CommonFields ++ RececdFields}, [native_utf8]),
    RiakObj = meddatum:maybe_new_ro(C, Bucket, list_to_binary(Key), JSONRecords),
    riakc_pb_socket:put(C, RiakObj).
