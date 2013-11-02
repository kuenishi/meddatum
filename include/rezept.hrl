-include("rezept_codes.hrl").
-include("rezept_med.hrl").
-include("rezept_dpc.hrl").

-type string_date() :: string().

%% -define(TYPE_CONVERSION,
%%         [{integer, "数字"},
%%          {latin1, "英数"},
%%          {unicode, "漢字"},
%%          {date, "数字"} %% GMMYY
%%         ]). %% or {maybe, Type}


-define(RECORD_TYPES,
        [
         {"IR", "医療機関情報レコード", ?COLUMNS_IR}, %% 保険医療機関単位データの先頭に記録必須
         {"RE", "レセプト共通レコード", ?COLUMNS_RE}, %% レセプト単位データの先頭に記録必須
         {"HO", "保険者レコード", ?COLUMNS_HO},       %% 医療保険レセプトの場合に記録
         {"KO", "公費レコード", ?COLUMNS_KO},         %% 公費負担医療レセプトの場合に記録
         {"KH", "国保連固有情報レコード", ?COLUMNS_KH},%% 国保連固有情報の場合に記録

         %% from DPC
         {"BU", "診断群分類レコード", ?COLUMNS_BU},   %% 診断群分類情報は、レセプトがDPCレセプト及び総括対象DPCレセプトの場合に記録。
         {"SB", "傷病レコード", ?COLUMNS_SB}, %% レセプトがDPCレセプト及び総括対象DPCレセプトの場合に記録する。
         {"KK", "患者基礎レコード", ?COLUMNS_KK},
         {"SK", "診療関連レコード", ?COLUMNS_SK},
         {"GA", "外泊レコード", ?COLUMNS_GA},
         {"HH", "包括評価レコード", ?COLUMNS_HH},
         {"GT", "合計調整レコード", ?COLUMNS_GT},
         {"CD", "合計調整レコード", ?COLUMNS_CD},

         {"SY", "傷病名レコード", ?COLUMNS_SY},       %% 傷病名を記録
         {"SI", "診療行為レコード", ?COLUMNS_SI},     %% SI 診療行為を記録
         {"IY", "医薬品レコード ", ?COLUMNS_IY},      %% IY 医薬品を記録
         {"TO", "特定器材レコード", ?COLUMNS_TO},     %% TO 特定器材を記録
         {"CO", "コメントレコード", ?COLUMNS_CO},     %% CO コメントを記録
         {"NI", "日計表レコード", ?COLUMNS_NI},       %% NI 摘要情報の日毎の回数を記録
         {"SJ", "症状詳記レコード", ?COLUMNS_SJ},     %% SJ 症状詳記を記録
         {"TI", "臓器提供医療機関情報レコード", ?COLUMNS_TI},
         %%  TI 臓器提供医療機関単位データの先頭に記録必須
         {"TR", "臓器提供者レセプト情報レコード", ?COLUMNS_TR},
         %%  TR 臓器提供者レセプト単位データの先頭に記録必須
         {"TS", "臓器提供者請求情報レコード", ?COLUMNS_TS},
         %%  TS 臓器提供者レセプトの請求情報として記録必須
         {"GO", "診療報酬請求書レコード", ?COLUMNS_GO},%% GO 医療機関単位データの最後に記録必須

         %% from rezept01.pdf
         {"HI", "返戻医療機関レコード", ?COLUMNS_HI}, %% HI 保険医療機関単位データの先頭に記録必須
         {"HR", "返戻理由レコード", ?COLUMNS_HR},     %% HR 返戻理由を記録
         %% HR 履歴管理情報の付された返戻理由レコードを記録
         %% 履歴請求データ RE等 履歴管理情報の付された※請求データを記録

         {"RC", "レコード管理情報レコード", ?COLUMNS_RC},
         %% RC 審査支払機関が当該レセプトを識別する情報を記録
         {"HG", "返戻合計レコード", ?COLUMNS_HG},  %% HG 保険医療機関単位データの最後に記録必須
         {"JY", "事由レコード", ?COLUMNS_JY},    %% JY 補正箇所と補正事由を記録
         {"EX", "審査運用レコード", ?COLUMNS_EX},
         %% EX 審査支払機関による運用で付加する情報を記録
         {"MD", "再審査等申し出レコード", ?COLUMNS_MD}, %% MD 再審査等申し出理由を記録
         {"RT", "理由対象レコード", ?COLUMNS_RT},     %% RT 申し出理由の対象を記録
         {"JR", "レセプト縦覧レコード", ?COLUMNS_JR}, %% JR 関連するレセプトの検索番号等を記録
         {"MK", "再審査等申し出結果レコード", ?COLUMNS_MK},
         %% MK 審査支払機関での再審査等結果を記録
         {"MN", "レセプト管理レコード", ?COLUMNS_MN} %% MN レセプト共通キーなどの情報を記録

        ]).
