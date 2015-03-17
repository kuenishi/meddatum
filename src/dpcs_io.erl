-module(dpcs_io).

-include("meddatum.hrl").
-include_lib("riakc/include/riakc.hrl").

-export([put_json/5]).

put_json(C, Mode, HospitalID, _Date, {Key0, CommonFields, RececdFields}) ->
    BucketName = Mode ++ ":" ++ HospitalID,
    Bucket = meddatum:true_bucket_name(list_to_binary(BucketName)),
    JSONRecords = jsone:encode({CommonFields ++ RececdFields}, [native_utf8]),
    Key = iolist_to_binary(Key0),
    RiakObj = meddatum:maybe_new_ro(C, Bucket, Key, JSONRecords),
    riakc_pb_socket:put(C, RiakObj).
