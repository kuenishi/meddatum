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

%% ア 医療機関情報
-define(COLUMNS_IR,
        [
         {record_info, latin1, 2}, %% 固定 “IR”を記録する。
         {shinsakb, integer, 2}, %% 固定 審査支払機関コード(別表1)を記録す る。
         {state, integer, 2},    %% 固定 保険医療機関の所在する都道府県コード (別表2)を記録する。
         {tenhyo, integer, 1}, %% 固定 保険医療機関が使用する点数表コード(別 表3)を記録する。
         {cocd, integer, 7},%% 固定 保険医療機関について定められた医療機関 コードを記録する。
         {yobi, {maybe, integer}, 2}, %%  数字  2 可変 記録を省略する。
         {hosp_nm, unicode, 40}, %% 漢字  40 可変
         %% 1 地方厚生(支)局長に届け出た保険医療 機関の名称を記録する。
         %% 2 保険医療機関名称が40バイトに満たな い場合は、後続する“スペース”を省略し ても差し支えない。
         {seiym, gyymm, 5}, %% 数字  5 固定
         %% 1 請求年月を和暦で年号区分コード(別表 4)を含めた形で記録する。
         %% 2 数字“GYYMM”の形式で記録する。

         {multi_vol, integer, 2}, %% 固定 審査支払機関へ提出するボリューム単位毎 に“00”から昇順に2桁の連続番号を記録す る。
         {tel, {maybe, latin1}, 15} %% 英数  15  可変
         %% 1 保険医療機関の電話番号を記録する。
         %% 2 電話番号は市外局番、市内局番及び加入者番号を記録する。この場合において、各 番号の間にはカッコ又はハイフンを用いる ことができる。
         %% 3 電話番号が 15 バイトに満たない場合は、後続する“スペース”を省略しても差 し支えない。
         %% 4 電話番号の記録は任意であり、記録しな い場合は記録を省略する。
        ]).

%%       (ウ) 国保連固有情報レコード
-define(COLUMNS_KH,
        [
         {record_info, latin1, 2},%  英数 2 固定“KH”を記録する。
         {ko_info, unicode, 100}%  英数 又は 漢字  100  可変
         %% 1 任意のフォーマットとする。
         %% 2 全体で100バイトとする。
        ]).


-include("rezept_med.hrl").
-include("rezept_dpc.hrl").
