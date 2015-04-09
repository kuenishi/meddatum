
%% SEE md_record.erl when you're changing these names
%% these are just prefix for bucket name
-define(SSMIX_BUCKET, <<"ssmix">>).
-define(RECEPT_BUCKET, <<"recept">>).
-define(DPCS_BUCKET, <<"dpcs">>).

-define(BUCKET_NAME_SEPARATOR, <<":">>).
-define(BUCKET_TYPE, <<"md">>).
-define(SCHEMA_NAME, <<"md_schema">>).
-define(INDEX_NAME, <<"md_index">>).

-define(APPLICATION_JSON, "application/json").

-define(DPCS_TRAIL_MARKER, <<"95">>). %% McQueen

%% a bucket to store date-independent ssmix records
%% which are:
%% - ADT^A08 患者基本情報の更新
%% - ADT^A23 患者基本情報の削除
%% - ADT^A54 担当医の変更
%% - ADT^A55 担当医の取消
%% - ADT^A60 アレルギー情報の登録／更新
%% - PPR^ZD1 病名(歴)情報の登録／更新
-define(SSMIX_PATIENTS_BUCKET, <<"ssmix-patients">>).

%% meddatum context
-record(context, {
          logger :: pid(),
          riakc :: pid(),
          config = [] :: proplists:proplist()
         }).

