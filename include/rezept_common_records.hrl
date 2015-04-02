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


%% (エ) コメントレコード
-define(COLUMNS_CO,
        [
         {record_info, latin1, 2},% 英数  2 固定“CO”を記録する。
         {shin_identifier, {maybe, integer}, 2},%  数字 2 可変
         %% 1 診療識別コード(別表19)を記録す る。
         %% 2 診療識別を必要としないコメントの場合 は、記録を省略する。
         {ftnkb, latin1, 1},%  英数 1 固定 負担区分コード(別表20)を記録する。
         {commentcd, integer, 9},%  数字 9 固定
         %% 別に定めるコメントコードを記録する。
         {mojidata, {maybe, unicode}, 76}%  漢字  76  可変
         %%  1 各コメントコードに対応して、文字情 報、数字情報又は別に定める修飾語コード を記録する。
         %% 2 文字データの記録を要しないコメントコ ードの場合は、記録を省略する。
         %% 3 記録する文字データが76バイトに満た ない場合は、後続する“スペース”を省略 しても差し支えない。
        ]).


%% カ 症状詳記情報      症状詳記レコード
-define(COLUMNS_SJ,
        [
         {record_info, latin1, 2},%  英数 2 固定 “SJ”を記録する。
         {shoujyou_kb, {maybe, integer}, 2},%  数字 2 可変
         %% 1 症状詳記区分コード(別表22)を記録 する。
         %% 2 同一症状詳記区分の症状詳記データを複 数レコードにまたがって記録する場合は、 後続レコードの症状詳記区分の記録を省略 する。

         {shoujyou_data, unicode, 2400}%  漢字 2400 可変
         %%  1 症状詳記を記録することができる。
         %% 2 記録する文字データが2400バイトに 満たない場合は、後続する“スペース”を 省略しても差し支えない。
         %%        注1 同一の症状詳記区分を複数記録する場合は、症状詳記の順に記録する。
         %%  2 症状詳記データ内で段落を分ける場合は、段落ごとに複数の症状詳記レコードに分けて記録する。
        ]).

%% エ 傷病名情報
-define(COLUMNS_SY,
        [
         {record_info, latin1, 2},%  英数 2 固定“SY”を記録する。
         {rececd, latin1, 7},%  数字 7 固定
         %% 1 別に定める傷病名コードを記録する。
         %% 2 未コード化傷病名については“0000999 ”を記録する。

         {shinkymd, gyymmdd, 7},%  数字 7 固定
         %% 1 保険診療を開始した年月日を和暦で年号 区分コード(別表4)を含めた形で記録す る。
         %% 2 数字“GYYMMDD”の形式で記録する。

         {tenkikb, integer, 1},%  数字 1 固定
          %% 該当する転帰区分コード(別表17)を記 録する。

         {modcd, {maybe, latin1}, 80},%  英数 80 可変
         %% 1 傷病名コードで規定している傷病名に接 頭語又は接尾語を必要とする場合は、別に 定める修飾語コードを順に記録すること。 ただし、最大20個までの記録を限度とす る。
         %% 2 記録する際には、必ず4の倍数のバイト 数となる。
         %% 3 記録する修飾語コードが80バイトに満 たない場合は、後続する“スペース”を省 略しても差し支えない。
          %% 4 その他の場合は、記録を省略する。

         {sbyonm, {maybe, unicode}, 40},%  漢字 40 可変
         %% 1 未コード化傷病名の場合は、当該傷病名 を記録する。
         %% 2 傷病名称が40バイトに満たない場合 は、後続する“スペース”を省略しても差 し支えない。
         %% 3 その他の場合は、記録を省略する。

         {shusbyo, {maybe, integer}, 2},%  数字 2 可変
         %% 1 主傷病の場合は、主傷病コード(別表1 8)を記録する。
         %% 2 その他の場合は、記録を省略する。

         {hskcmt, {maybe, unicode}, 40}%  漢字  40  可変
          %%  1 傷病名に対する補足コメントが必要な場 合に記録する。
          %% 2 その他の場合は、記録を省略する。
          %%            注 GYYMMDDのGは年号区分コード(別表4)、YYは和暦年、MMは月、DDは日を示す。
        ]).


%% (ア) 診療行為レコード
-define(COLUMNS_SI,
        [
         {record_info, latin1, 2},%  英数 2 固定 “SI”を記録する。
         {shin_identifier, {maybe, integer}, 2},%  数字 2 可変
         %% 1 診療識別コード(別表19)を記録す る。
         %% 2 診療識別を必要としない診療行為の場合 は、記録を省略する。

         {ftnkb, integer, 1},%  英数 1 固定 負担区分コード(別表20)を記録する。
         {rececd, latin1, 9},%  数字 9 固定 別に定める診療行為コードを記録する。
         {ryodata, {maybe, integer}, 8},%  数字 8 可変
          %% 1 数量データを必要とする診療行為の場合 は、診療行為コードで規定している単位で 整数値を記録する。
          %% 2 有効桁数が8桁に満たない場合は、有効 桁までの記録としても差し支えない。
          %% 3 数量データを必要としない診療行為の場 合は、記録を省略する。

         {actten, {maybe, integer}, 7},%  数字 7 可変
          %% 1 診療行為の点数又は金額を記録する。
          %% 2 有効桁数が7桁に満たない場合は、有効 桁までの記録としても差し支えない。
          %% 3 点数又は金額を記録しない場合は、記録 を省略する。
          %%   点数・回数算定単 位内の最終レコー ドのみ記録する。

         {actcnt, integer, 3},%  数字 3 可変
         %% 1 診療行為の回数を記録する。
         %% 2 有効桁数が3桁に満たない場合は、有効 桁までの記録としても差し支えない。
         %%   1 点数・回数算 定単位内の回数 は、同一の回数 を記録する。
         %% 2 回数は、算定 日情報の1日の 情報から31日 の情報の合計値 と一致する。た だし、平成24 年3月診療以前 分については、 その限りでな い。

         {commentcd1, {maybe, integer}, 9},%  数字 9 可変
         {mojidata1, {maybe, unicode}, 100},%  漢字 100 可変
         {commentcd2, {maybe, integer}, 9},%  数字 9 可変
         {mojidata2, {maybe, unicode}, 100},%  漢字 100 可変
         {commentcd3, {maybe, integer}, 9},%  数字 9 可変
         {mojidata3, {maybe, unicode}, 100},%  漢字 100 可変
         %% 1 コメントが必要な場合、別に定めるコメ ントコードと文字データを順次、対で記録 する。
         %% 2 文字データは対となるコメントコードに対応して、文字情報数字情報又は別に定める修飾語コードを記録する。
         %% 3 文字データの記録を要しないコメントコードの場合は、文字データの記録を省略す る。
         %% 4 記録する文字データが100バイトに満たない場合は、後続する“スペース”を省略しても差し支えない。
         %% 5 修飾語コードを記録する場合、最大5コ ードまでを前詰めで記録する。
         %% 6 コメントを記録しない場合は、コメント コードと文字データの記録を省略する。
         %%            コメントが3対に満たない場合は、1より順次記録する。

          %%  1 回数を記録する。
          %% 2 回数を記録しない場合は、記録を省略す
          %%  る。
          %%  1 点数・回数算 定単位内の算定 日情報は、同一 日に同一回数を 記録する。
          %% 2 平成24年4 月診療以降分の 場合は、記録を 必須とする。た だし、他医療機 関に係る臓器提 供者レセプトに ついては、省略 しても差し支え ない。
          %% 3 平成24年3 月診療以前分の 場合は、記録を 省略しても差し 支えない。ただ し、診療報酬明 細書の記載要領 の各規定により 摘要欄に算定日 を記載すること とされている項 目については、 コメントに記録 する。
          %% 4 算定日情報の 1日の情報から 31日の情報の 合計値は、回数 と一致する。た だし、平成24 年3月診療以前 分については、 その限りでな い。
          %%  1日の情報  数字 3 可変
          %%  2日の情報  数字 3 可変
          %% ...
          %% 30日の情報  数字 3 可変
          %% 31日の情報  数字 3  可変
         {info_1, {maybe, integer}, 3},         {info_2, {maybe, integer}, 3},
         {info_3, {maybe, integer}, 3},         {info_4, {maybe, integer}, 3},
         {info_5, {maybe, integer}, 3},         {info_6, {maybe, integer}, 3},
         {info_7, {maybe, integer}, 3},         {info_8, {maybe, integer}, 3},
         {info_9, {maybe, integer}, 3},         {info_10, {maybe, integer}, 3},
         {info_11, {maybe, integer}, 3},        {info_12, {maybe, integer}, 3},
         {info_13, {maybe, integer}, 3},        {info_14, {maybe, integer}, 3},
         {info_15, {maybe, integer}, 3},        {info_16, {maybe, integer}, 3},
         {info_17, {maybe, integer}, 3},        {info_18, {maybe, integer}, 3},
         {info_19, {maybe, integer}, 3},        {info_20, {maybe, integer}, 3},
         {info_21, {maybe, integer}, 3},        {info_22, {maybe, integer}, 3},
         {info_23, {maybe, integer}, 3},        {info_24, {maybe, integer}, 3},
         {info_25, {maybe, integer}, 3},        {info_26, {maybe, integer}, 3},
         {info_27, {maybe, integer}, 3},        {info_28, {maybe, integer}, 3},
         {info_29, {maybe, integer}, 3},        {info_30, {maybe, integer}, 3},
         {info_31, {maybe, integer}, 3}
         ]).


%% (イ) 医薬品レコード
-define(COLUMNS_IY,
        [
         {record_info, latin1, 2},%  英数 2 固定“IY”を記録する。
         {shin_identifier, {maybe, integer}, 2},%  数字 2 可変
         %% 1 診療識別コード(別表19)を記録す る。
         %% 2 診療識別を必要としない医薬品の場合 は、記録を省略する。

         {ftnkb, latin1, 1},%  英数 1 固定 負担区分コード(別表20)を記録する。
         {rececd, latin1, 9},%  数字 9 固定 別に定める医薬品コードを記録する。
         {ryo, {maybe, latin1}, 11},%  英数 11 可変
         %% 1 医薬品の使用量は、整数部5桁、小数部5桁として、整数部と小数部は小数点で区 切り記録する。
         %% 2 有効桁数が11桁に満たない場合は、有 効桁までの記録としても差し支えない。
         %% 3 使用量を記録しない場合は、記録を省略 する。

          %% same as 'SI' below
         {actten, {maybe, integer}, 7},%  数字 7 可変
         {actcnt, integer, 3},%  数字 3 可変
         {commentcd1, {maybe, integer}, 9},%  数字 9 可変
         {mojidata1, {maybe, unicode}, 100},%  漢字 100 可変
         {commentcd2, {maybe, integer}, 9},%  数字 9 可変
         {mojidata2, {maybe, unicode}, 100},%  漢字 100 可変
         {commentcd3, {maybe, integer}, 9},%  数字 9 可変
         {mojidata3, {maybe, unicode}, 100},%  漢字 100 可変
         {info_1, {maybe, integer}, 3},         {info_2, {maybe, integer}, 3},
         {info_3, {maybe, integer}, 3},         {info_4, {maybe, integer}, 3},
         {info_5, {maybe, integer}, 3},         {info_6, {maybe, integer}, 3},
         {info_7, {maybe, integer}, 3},         {info_8, {maybe, integer}, 3},
         {info_9, {maybe, integer}, 3},         {info_10, {maybe, integer}, 3},
         {info_11, {maybe, integer}, 3},        {info_12, {maybe, integer}, 3},
         {info_13, {maybe, integer}, 3},        {info_14, {maybe, integer}, 3},
         {info_15, {maybe, integer}, 3},        {info_16, {maybe, integer}, 3},
         {info_17, {maybe, integer}, 3},        {info_18, {maybe, integer}, 3},
         {info_19, {maybe, integer}, 3},        {info_20, {maybe, integer}, 3},
         {info_21, {maybe, integer}, 3},        {info_22, {maybe, integer}, 3},
         {info_23, {maybe, integer}, 3},        {info_24, {maybe, integer}, 3},
         {info_25, {maybe, integer}, 3},        {info_26, {maybe, integer}, 3},
         {info_27, {maybe, integer}, 3},        {info_28, {maybe, integer}, 3},
         {info_29, {maybe, integer}, 3},        {info_30, {maybe, integer}, 3},
         {info_31, {maybe, integer}, 3}
        ]).

-define(COLUMNS_MN,
        [
         {record_info, latin1, 2}, %% 固定 “MN”を記録する。
         {<<"レセプト管理番号">>, integer, 16}, %% 可変 数字“XXXXXXXXXXXXXXXX”の形式で記録する。 (9~16桁で構成する。)
         {<<"保険医療機関の所在地">>, unicode, 80}, %% 可変
          %% 1 保険医療機関の所在地を記録する。
          %% 2 保険医療機関の所在地が80バイトに満たない場合、 後続する“スペース”の記録は省略する

         {yobi, {maybe, integer}, 30}, %% 可変 記録は省略する。
         {yobi, {maybe, integer}, 1},  %% 可変 記録は省略する。
         {yobi, {maybe, integer}, 1},  %% 可変 記録は省略する。
         {yobi, {maybe, unicode}, 100}
        ]).

-define(REZEPT_COMMON_RECORDS,
        [
         {"IR", "医療機関情報レコード", ?COLUMNS_IR}, %% 保険医療機関単位データの先頭に記録必須

         {"KH", "国保連固有情報レコード", ?COLUMNS_KH},%% 国保連固有情報の場合に記録
         {"CO", "コメントレコード", ?COLUMNS_CO},     %% CO コメントを記録
         {"SJ", "症状詳記レコード", ?COLUMNS_SJ},     %% SJ 症状詳記を記録

         {"SY", "傷病名レコード", ?COLUMNS_SY},       %% 傷病名を記録
         {"SI", "診療行為レコード", ?COLUMNS_SI},     %% SI 診療行為を記録
         {"IY", "医薬品レコード ", ?COLUMNS_IY},      %% IY 医薬品を記録
         {"TO", "特定器材レコード", ?COLUMNS_TO},     %% TO 特定器材を記録
         {"NI", "日計表レコード", ?COLUMNS_NI},       %% NI 摘要情報の日毎の回数を記録


         {"TI", "臓器提供医療機関情報レコード", ?COLUMNS_TI},
         %%  TI 臓器提供医療機関単位データの先頭に記録必須
         {"TR", "臓器提供者レセプト情報レコード", ?COLUMNS_TR},
         %%  TR 臓器提供者レセプト単位データの先頭に記録必須
         {"TS", "臓器提供者請求情報レコード", ?COLUMNS_TS},
         %%  TS 臓器提供者レセプトの請求情報として記録必須
         {"GO", "診療報酬請求書レコード", ?COLUMNS_GO}, %% GO 医療機関単位データの最後に記録必須

         %% %% from rezept01.pdf
         %% {"HI", "返戻医療機関レコード", ?COLUMNS_HI}, %% HI 保険医療機関単位データの先頭に記録必須
         %% {"HR", "返戻理由レコード", ?COLUMNS_HR},     %% HR 返戻理由を記録
         %% %% HR 履歴管理情報の付された返戻理由レコードを記録
         %% %% 履歴請求データ RE等 履歴管理情報の付された※請求データを記録

         {"RC", "レコード管理情報レコード", ?COLUMNS_RC},
         %% RC 審査支払機関が当該レセプトを識別する情報を記録
         %% {"HG", "返戻合計レコード", ?COLUMNS_HG},  %% HG 保険医療機関単位データの最後に記録必須
         {"JY", "事由レコード", ?COLUMNS_JY},    %% JY 補正箇所と補正事由を記録
         {"EX", "審査運用レコード", ?COLUMNS_EX},
         %% %% EX 審査支払機関による運用で付加する情報を記録
         %% {"MD", "再審査等申し出レコード", ?COLUMNS_MD}, %% MD 再審査等申し出理由を記録
         %% {"RT", "理由対象レコード", ?COLUMNS_RT},     %% RT 申し出理由の対象を記録
         %% {"JR", "レセプト縦覧レコード", ?COLUMNS_JR}, %% JR 関連するレセプトの検索番号等を記録
         %% {"MK", "再審査等申し出結果レコード", ?COLUMNS_MK},
         %% %% MK 審査支払機関での再審査等結果を記録
         {"MN", "レセプト管理レコード", ?COLUMNS_MN} %% MN レセプト共通キーなどの情報を記録
        ]).
