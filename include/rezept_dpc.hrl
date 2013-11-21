%% records defined in iryokikan_in_03.pdf
%% オンライン又は光ディスク等による請求に係る記録条件仕様(DPC用)
%% via http://www.ssk.or.jp/rezept/iryokikan/download/files/iryokikan_in_03.pdf

%% キ
%% 診断群分類情報
%% 診断群分類情報は、レセプトがDPCレセプト及び総括対象DPCレセプトの場合に記録する。
-define(COLUMNS_BU,
        [
         {"レコード識別情報", latin1, 2}, %% 英数 2 固定 BUを記録する。
         {"診断群分類番号", latin1, 14}, %% 英数 14 固定 別に定める診断群分類番号を記録する。
         {"今回入院年月日", gyymmdd, 7}, %% 数字 7 固定
         %% 1 今回入院年月日を和暦で年号区分コード(別表4)を含めた形で記録する。
         %% 2 数字GYYMMDDの形式で記録する。

         {"今回退院年月日", gyymmdd, 7}, %% 数字 7 可変
         %% 1 今回退院年月日を和暦で年号区分コード(別表4)を含めた形で記録する。
         %% 2 数字 GYYMMDD の形式で記録する。
         %% 3 退院前レセプト又は診断群分類区分の対象外に切り替わる前のレセプトについては記録を省略する。また、診断群分類番号の上6桁が同一である診断群分類での3日以内の再入院が行われた場合も記録を省略する。

         {"DPC転帰区分", {maybe, integer}, 1}, %% 数字 1 可変
         %% 1 退院時又は診断群分類区分の対象外に切り替わる場合、DPC転帰区分コード(別表21)を記録する。
         %% 2 その他の場合は、記録を省略しても差し支えない。

         {"死因", {maybe, unicode}, 100} %% 漢字 100 可変
         %% 1 DPC転帰区分が 7 (外死亡)の場合、死亡診断書に記入した死因を記録する。
         %% 2 記録する文字データが100バイトに満たない場合は、後続する スペース を省略しても差し支えない。
         %% 3 その他の場合は、記録を省略する。
        ]).

%% ク
%% 傷病情報
%% 傷病情報は、レセプトがDPCレセプト及び総括対象DPCレセプトの場合に記録する。
%% 傷病レコード
-define(COLUMNS_SB,
        [
         {"レコード識別情報", latin1, 2}, %% 英数 2 固定 SBを記録する。

         {"傷病名コード", integer, 7}, %% 数字 7 固定
         %% 1 別に定める傷病名コードを記録する。
         %% 2 傷病名コードを設定していない傷病名(以下「未コード化傷病名」という。)については 0000999 を記録する。

         {"修飾語コード", {maybe, latin1}, 80}, %% 英数 80 可変
         %% 1 傷病名コードで規定している傷病名に接頭語又は接尾語を必要とする場合は、別に定める修飾語コードを順に記録する。ただし、最大20個までの記録を限度とする。
         %% 2 記録する際には、必ず4の倍数のバイト数となる。
         %% 3 記録する修飾語コードが80バイトに満たない場合は、後続する スペース を省略しても差し支えない。
         %% 4 その他の場合は、記録を省略する。

         {"傷病名称", {maybe, unicode}, 40}, %% 漢字 40 可変
         %% 1 未コード化傷病名の場合は、当該傷病名を記録する。
         %% 2 傷病名称が40バイトに満たない場合は、後続する スペース を省略しても差し支えない。
         %% 3 その他の場合は、記録を省略する。

         {"ICD10コード", latin1, 5}, %% 英数 5 可変
         %% 1 傷病名に対応するICD10コードを記録する。
         %% 2 ICD10コードが5桁に満たない場合は、後続する スペース を省略しても差し支えない。

         {"傷病名区分", integer, 2}, %% 数字 2 固定 当該傷病名に対する傷病名区分コード(別表22)を記録する。

         {"死因", {maybe, integer}, 1}, %% 数字 1 可変
         %% 1 DPC転帰区分が 7 (外死亡)の場合、死因の対象傷病名について 1 (死因)を記録する。
         %% 2 その他の場合は、記録を省略する。

         {"補足コメント", {maybe, unicode}, 40} %% 漢字 40 可変
         %% 1 傷病名に対する補足コメントが必要な場合に記録する。
         %% 2 その他の場合は、記録を省略する。
        ]).

%% コ
%% 患者基礎情報
%% 患者基礎情報は、レセプトがDPCレセプト及び総括対象DPCレセプトの場合に記録する。
%% 患者基礎レコード
-define(COLUMNS_KK,
        [
         {"レコード識別情報", latin1, 2}, %% 固定 “KK”を記録する。

         {"予備", integer, 1}, %% 数字 1 可変 平 成 2 2 年3月 診 療以 前 分 の 場合、 当 時の 記 録 条 件仕様 に 基づき、「転科の有無」を記録する。

         {"一般病棟以外の病院移動の有無", {maybe, integer}, 1}, %% 数字 1 可変
         %% 1 対象外病棟への転科又は対象外病棟から一般病棟へ転棟した場合は、 1 (有)を記録する。
         %% 2 その他の場合は、記録を省略する。

         {"予定・緊急入院区分", integer, 1}, %% 数字 1 固定 予定・緊急入院区分コード(別表25)を記録する。

         {"前回退院年月日", {maybe, gyymmdd}, 7}, %% 数字 7 可変
         %% 1 当該保険医療機関において入院歴がある場合、前回の退院年月日を和暦で年号区分コード(別表4)を含めた形で記録する。ただし、診断群分類番号の上6桁が同一である診断群分類での3日以内の再入院が行われた場合、それ以前の退院年月日を記録する。
         %% 2 数字 GYYMMDD の形式で記録する。
         %% 3 その他の場合は、記録を省略する。

         {"前回同一傷病での入院の有無", {maybe, integer}, 1}, %% 数字 1 可変
         %% 1 当該保険医療機関において今回入院時の入院契機病名と前回入院時に最も医療資源を投入した傷病名が同一(診断群分類番号の上6桁が同一)である場合は、 1(有)を記録する。
         %% 2 その他の場合は、記録を省略する。

         {"入院時年齢", {maybe, integer}, 3}, %% 数字 3 可変
         %% 1 年齢要件又は月齢要件が定められている診断群分類区分の場合、入院時の患者年齢を記録する。
         %% 2 入院時の患者年齢が1歳未満で記録する場合、 0 を記録する。
         %% 3 有効桁数が3桁に満たない場合は、有効桁までの記録としても差し支えない。
         %% 4 その他の場合は、記録を省略しても差し支えない。

         {"出生時体重", {maybe, integer}, 4}, %% 数字 4 可変
         %% 1 出生時体重要件が定められている診断群分類区分の場合、g単位で患者体重を記録する。
         %% 2 出生時体重が不明の場合は、記録する。
         %% 3 有効桁数が4桁に満たない場合は、有効桁までの記録としても差し支えない。
         %% 4 その他の場合は、記録を省略しても差し支えない。

         {"JCS", {maybe, integer}, 3}, %% 数字 3 可変
         %% 1 JCS要件が定められている診断群分類区分の場合、JCSを記録する。
         %% 2 有効桁数が3桁に満たない場合は、有効桁までの記録としても差し支えない。
         %% 3 その他の場合は、記録を省略しても差し支えない。

         {"予備", {maybe, integer}, 1}, %% 数字 1 可変 記録を省略する。

         {"Burn_Index", {maybe, latin1}, 5}, %% 英数 5 可変
         %% 1 Burn Index要件が定められている診断群分類区分の場合、Burn Indexを 記 録 す る。
         %% 2 Burn Indexは、整数部3桁、小数部1桁として、整数部と小数部は小数点で区切り記録する。
         %% 3 有効桁数が5桁に満たない場合は、有効桁までの記録としても差し支えない。
         %% 4 その他の場合は、記録を省略しても差し支えない。

         {"重症度等", {maybe, unicode}, 100},%% 漢字 100 可変
         %% 1 重症度等の要件が定められている診断群分類区分の場合、重症度等を記録する。
         %% 2 有効桁数が100バイトに満たない場合は、有効桁までの記録としても差し支えない。
         %% 3 その他の場合は、記録を省略しても差し支えない。

         {"予備", {maybe, integer}, 3}, %% 数字 3 可変
         %% 1 GAF要件が定められている診断群分類区分の場合、GAFを 記 録 す る。
         %% 2 有効桁数が3桁に満たない場合は、有効 の 桁までの記録としても差し支えない。
         %% 3 その他の場合は、記録を省略しても差し 支えない。
         %% 平成22年3月診療以前分の場合、当時記 録 条 件 仕 様 に 基づき、「GAF」を記録する。

         {"入院時月齢", {maybe, integer}, 2} %% 数字 2 可変
         %% 1 月齢要件が定められている診断群分類区分で、入院時の患者年齢が1歳未満の場合、入院時の患者月齢を記録する。
         %% 2 入院時の患者年齢が1歳以上の場合は、記録を省略する。
         %% 3 記録する場合は、 0から11までの整数を記録する。
         %% 4 有効桁数が2桁に満たない場合は、有効桁までの記録としても差し支えない。
         %% 5 その他の場合は、記録を省略しても差し支えない。
        ]).


%% サ
%% 診療関連情報
%% 診療関連情報は、レセプトがDPCレセプト及び総括対象DPCレセプトの場合に記録する。
%% 診療関連レコード
-define(COLUMNS_SK,
        [
         {"レコード識別情報", latin1, 2}, %% 固定 “SK”を記録する。

         {"診療行為コード", {maybe, integer}, 9}, %% 数字 9 可変
          %% 1 手術又は処置等の診療名称を省略する場合、別に定める診療行為コードを記録する。 診療行為コードの記録は任意とする。
          %% 2 診療行為コードが定められていない場合は、記録を省略する。


          %% 1 区分番号又は診療区分コードのいずれかを記録し、他方は記録を省略する。
          %% 2 区分番号は、平成24年3月診療以前分の場合、当時の記録条件仕様に基づき最大7バイトで記録する。
         %%{"区分番号", {maybe, latin1}, 10}, %% 英数 10 可変
         %% actually,       ________ this should be latin1 according to the spec but example data included 'K9202イ'.
         {"区分番号", {maybe, unicode}, 10}, %% 英数 10 可変
          %% 1 厚生労働大臣が定める傷病名、手術、処置等及び副傷病名を定める件(平成20年厚生労働省告示第95号)で、手術、手術・処置等1及び手術・処置等2に定められた区分を記録する。
          %% 2 有効桁数が10桁に満たない場合は、有効桁までの記録としても差し支えない。
          %% 3 区分番号が定められていない場合は、記録を省略する。

          {"実施(予定)年月日", {maybe, gyymmdd}, 7}, %% 数字 7 可変
          %% 1 手術、処置の実施年月日又は予定年月日を和暦で年号区分コード(別表4)を含めた形で記録する。
          %% 2 数字 GYYMMDD の形式で記録する。
          %% 3 その他の場合は、記録を省略しても差し 支えない。

          {"予備", {maybe, integer}, 1}, %% 数字 1 可変 記録を省略する。

          {"診療区分コード", {maybe, latin1}, 4}, %% 英数 4 可変
          %% 1 区分番号が定められていない診断群分類区分の場合、診療区分コード(別表26)を記録する。
          %% 2 その他の場合は、記録を省略する。

          {"診療名称", {maybe, unicode}, 200} %% 漢字 200 可変
          %% 1 区分番号が記録された場合、区分番号の翻訳情報を記録する。ただし、診療行為コード又は診療区分コードが記録された場合は記録を省略する。
          %% 2 診療名称が200バイトに満たない場合は後続する スペース を省略しても差し支えない。
          %% 3 その他の場合は、記録を省略する。
         ]).
%%% 2 診療行為コード、区分番号、診療区分コード(別表26)及び診療名称の記録は、次の組合せのいずれ か と す る 。

%% シ
%% 包括評価情報
%% 包括評価情報は、レセプトがDPCレセプト及び総括対象DPCレセプトの場合に記録する。
%% (ア) 外泊レコード
-define(COLUMNS_GA,
        [
         {"レコード識別情報", latin1, 2}, %% 固定 “GA”を記録する。

         {"診療年月", gyymm, 5}, %% 数字 5 固定
         %% 1 当該外泊レコードの診療年月を和暦で年号区分コード(別表4)を含めた形で記録する。
         %% 2 数字 GYYMM の形式で記録する。

         {"請求調整区分", integer, 1}, %% 数字 1 固定 請求調整区分コード(別表27)を記録する。

         %% {"外泊等", {maybe, integer}, 31}, %% 数字 31 可変
         %% DAMN this is not integer but a char list, 10^31 should overflow in usual langs
         {"外泊等", {maybe, latin1}, 31}, %% 数字 31 可変
         %% 1 当該診療年月の外泊の状況及び3日以内の再入院の状況を外泊等コード(別表28)で日々単位に記録する。
         %% 2 当該診療月が31日に満たない月の場合は、月末日までの情報を記録し、残りは記録を省略する。
         %% 3 総括対象DPCレセプトの場合、当該明細情報の入院期間に係る情報に限り記録する。

         {"診断群分類番号", {maybe, latin1}, 14}, %% 英数 14 可変
         %% 1 請求調整区分が 1 (過去の請求済分)であり診断群分類レコードの診断群分類番号と異なる場合、当該診療年月時点の診断群分類番号を記録する。
         %% 2 その他の場合は、記録を省略しても差し支えない。

         {"医療機関別係数", latin1, 6}, %% 英数 6 可変
         %% 1 病院ごとの調整係数と当該診療年月時点の施設基準等による係数を合算又は減算して得た係数を記録する。
         %% 2 医療機関別係数は、整数部1桁、小数部4桁として、整数部と小数部は小数点で区切り記録する。
         %% 3 有効桁数が6桁に満たない場合は、有効桁までの記録としても差し支えない。

         {"翌月再入院予定の有無", {maybe, integer}, 1} %% 数字 1 可変
         %% 1 請求調整区分が 2 (今月の請求分)であり、当該診療年月の月末日に退院した後、3日以内に上6桁が同一である診断群分類による再入院が行われる予定がある場合、 1 (有)を記録する。
         %% 2 その他の場合は、記録を省略する。
        ]).

%% GYYMMのGは年号区分コード(別表4)、YYは和暦年、MMは月を示す。
%% 総括レセプトで月初に退院、月末に再入院した場合、月初退院に係る明細情報の外泊レコードには月初の入院期間の外泊状況を記録する。また月末再入院に係る明細情報の外泊レコードには月末の入院期間の外泊状況を記録する。

%% (イ) 包括評価レコード
-define(COLUMNS_HH,
        [
         {"レコード識別情報", latin1, 2}, %% 固定 “HH”を記録する。

         {"診療年月", gyymm, 5}, %% 数字 5 固定
         %% 1 当該包括評価レコードの診療年月を和暦で年号区分コード(別表4)を含めた形で記録する。
         %% 2 数字 GYYMM の形式で記録する。

         {"請求調整区分", integer, 1}, %% 数字  1 固定 請求調整区分コード(別表27)を記録する。

         {"自他保険区分", {maybe, integer}, 1},%% 数字 1 可変
         %% 1 請求調整区分が 2 (今月の請求分)の場合、自他保険区分コード(別表29)を記録する。
         %% 2 その他の場合は、記録を省略しても差し支えない。

         {"負担区分", {maybe, integer}, 1}, %% 英数 1 可変
         %% 1 請求調整区分が 2 (今月の請求分)であり自他保険区分が 1 (自保険分)の場合、負担区分コード(別表19)を記録する。
         %% 2 その他の場合は、記録を省略しても差し支えない。

         {"入院期間区分", integer, 1}, %% 数字 1 固定
         %% 診療年月、請求調整区分、自他保険区分及び 1 固定 負担区分に対応する入院期間区分コード(別表30)を記録する。

         {"入院期間区分別点数", integer, 6}, %% 数字 6 可変
         %% 1 診断群分類区分及び入院期間区分に対応した1日当たりの包括評価点数を記録する。
         %% 2 有効桁数が6桁に満たない場合は、有効桁までの記録としても差し支えない。

         {"入院期間区分別入院日数", integer, 2}, %% 数字 2 可変
         %% 1 診療年月、請求調整区分、自他保険区分、負担区分及び入院期間区分に対応する入院期間から外泊日数を除いた入院日数を記録する。
         %% 2 有効桁数が2桁に満たない場合は、有効桁までの記録としても差し支えない。

         {"包括小計点数", integer, 7} %% 数字 7 可変
         %% 1 診療年月、請求調整区分、自他保険区分、負担区分及び入院期間区分に対応する包括小計点数を記録する。
         %% 2 有効桁数が7桁に満たない場合は、有効桁までの記録としても差し支えない。
        ]).
%% 1 GYYMMのGは年号区分コード(別表4)、YYは和暦年、MMは月を示す。
%% 2 退院月に適用する診断群分類区分が入院期間中の診断群分類区分と異なる場合、退院月に係るレセプトにおいて調整分の包括評価レコードを記録する。
%% 3 入院期間区分別の入院日数が0日の診療月の場合は、当該入院期間区分に係る包括評価レコードを記録しない。

%% (ウ) 合計調整レコード
-define(COLUMNS_GT,
        [
         {"レコード識別情報", latin1, 2}, %% 固定 “GT”を記録する。

         {"診療年月", gyymm, 5}, %% 数字 5 固定
         %% 1 当該合計調整レコードの診療年月を和暦で年号区分コード(別表4)を含めた形で記録する。
         %% 2 数字GYYMMの形式で記録する。

         {"請求調整区分", integer, 1}, %% 数字 1 固定 請求調整区分コード(別表27)を記録する。

         {"自他保険区分", {maybe, integer}, 1}, %% 数字 1 可変
         %% 1 請求調整区分が 2 (今月の請求分)の場合、自他保険区分コード(別表29)を記録する。
         %% 2 その他の場合は、記録を省略しても差し支えない。

         {"負担区分", {maybe, latin1}, 1}, %% 英数 1 可変
         %% 1 請求調整区分が 2 (今月の請求分)であり自他保険区分が 1 (自保険分)の場合、負担区分コード(別表19)を記録する。
         %% 2 その他の場合は、記録を省略しても差し支えない。

         {"包括小計点数合算", integer, 7}, %% 数字 7 可変
         %% 1 診療年月、請求調整区分、自他保険区分及び負担区分が一致する包括評価レコードの包括小計点数を合算して記録する。
         %% 2 有効桁数が7桁に満たない場合は、有効桁までの記録としても差し支えない。

         {"包括評価点数", integer, 7}, %% 数字 7 可変
         %% 1 包括小計点数合算と医療機関別係数の乗算結果を記録する。
         %% 2 有効桁数が7桁に満たない場合は、有効桁までの記録としても差し支えない。

         {"調整点数", {maybe, latin1}, 7}, %% 英数 7 可変
         %% 1 請求調整区分が 3 (過去の調整分)の場合、当該診療年月における調整分の包括評価点数と請求分の包括評価点数との差を記録する。
         %% 2 調整点数がマイナスの場合は、負符号(−)を付加して記録する。
         %% 3 有効桁数が7桁に満たない場合は、有効桁までの記録としても差し支えない。
         %% 4 その他の場合は、記録を省略する。

         {"今月包括合計点数", {maybe, latin1}, 8}, %% 英数 8 可変
         %% 1 今月請求する包括評価点数を記録する。
         %% 2 退院月において調整点数がある場合は、全調整点数及び当該包括評価点数を合算して記録する。
         %% 3 今月包括合計点数がマイナスの場合は、負符号(−)を付加して記録する。
         %% 4 有効桁数が8桁に満たない場合は、有効桁までの記録としても差し支えない。
         %% 5 請求調整区分が 2 (今月の請求分)であり自他保険区分が 1 (自保険分)以外の場合は、記録を省略する。

         {"診療識別", {maybe, integer}, 2}, %% 数字 2 可変
         %% 1 請求調整区分が 2 (今月の請求分)であり自他保険区分が 1 (自保険分)の場合、診療識別コード(別表18) 93 を記録する。
         %% 2 その他の場合は、記録を省略する。

         {"変更年月日", {maybe, gyymmdd}, 7}, %% 数字 7 可変
         %% 1 保険者番号等の変更又は負担区分の変更があった場合、変更年月日を和暦で年号区分コード(別表4)を含めた形で記録する。
         %% 2 数字 GYYMMDD の形式で記録する。
         %% 3 その他の場合は、記録を省略する。

         {"文字データ", {maybe, unicode}, 40} %% 漢字 40 可変
         %% 1 変更年月日が記録された場合、変更情報を記録する。
         %% 2 記録する文字データが40バイトに満たない場合は、後続する スペース を省略しても差し支えない。
         %% 3 文字データの記録は任意であり、記録しない場合は記録を省略する。
        ]).
%% 注1 GYYMM(DD)のGは年号区分コード(別表4)、YYは和暦年、MMは月、DDは日を示す。
%% 2 診断群分類区分が変更された診療月については、調整点数が0点であっても調整分の内容を記録する。
%% 3 調整点数の記録可能範囲は、プラスの場合は符号なしで数字7桁まで、マイナスの場合は負符号1桁+数字6桁までとする。
%% 4 今月包括合計点数の記録可能範囲は、プラスの場合は符号なしで数字8桁まで、マイナスの場合は負符号1桁+数字7桁までとする。
%% 5 同一月に複数回の保険変更があった場合は、合計調整レコードごとに変更年月日を記録する。


%% セ
%% コーディングデータ情報
%% コーディングデータ情報は、レセプトがDPCレセプト及び総括対象DPCレセプトの場合、「診療識別の実施年月日順」に記録する。
%% コーディングデータレコード
-define(COLUMNS_CD,
        [
         {"レコード識別情報", latin1, 2}, %% 固定 “CD”を記録する。

         {"実施年月日", gyymmdd, 7}, %% 数字 7 固定
         %% 1 実施年月日を和暦で年号区分コード(別表4)を含めた形で記録する。
         %% 2 数字 GYYMMDD の形式で記録する。

         {"診療識別", integer, 2}, %% 数字 2 固定 診療識別コード(別表18)を記録する。
         {"順序番号", integer, 4}, %% 数字 4 可変 診療識別及び一連の行為毎に昇順に番号を記録する。
         {"行為明細番号", integer, 3}, %% 数字 3 可変 順序番号毎の行為明細単位に昇順に番号を記録する。
         {"レセプト電算処理システム用コード", integer, 9}, %% 数字 9 固定
         %% 診療行為コード(入院料、食事療養費、生活療養費及び標準負担額は除く)、医薬品コード又は特定器材コードを記録する。

         {"使用量", {maybe, latin1}, 11}, %% 英数 11 可変
         %% 1 医薬品又は特定器材の場合、使用量を記録する。
         %% 2 整数部5桁、小数部5桁として、整数部と小数部は小数点で区切り記録する。
         %% 3 有効桁数が11桁に満たない場合は、有効桁までの記録としても差し支えない。
         %% 4 使用量を必要としない場合及び診療行為の場合は、記録を省略する。

         {"数量データ", {maybe, integer}, 8}, %% 数字 8 可変
         %% 1 数量データの記録が必要な診療行為の場合、数量データを記録する。
         %% 2 有効桁数が8桁に満たない場合は、有効桁までの記録としても差し支えない。
         %% 3 数量データを必要としない場合及び医薬品又は特定器材の場合は、記録を省略する。

         {"単位コード", {maybe, integer}, 3}, %% 数字 3 可変
          %% 1 特定器材の場合、特定器材単位コード(別表31)を記録する。
          %% 2 単位が規定されている特定器材コードの場合は、記録を省略しても差し支えない。
          %% 3 酸素の補正率等使用量がない場合は、記録を省略する。
          %% 4 診療行為又は医薬品の場合は、記録を省略する。

          {"回数", integer, 3}, %% 数字 3 可変
          %% 1 診療行為、医薬品及び特定器材の回数を記録する。
          %% 2 有効桁数が3桁に満たない場合は、有効桁までの記録としても差し支えない。

          {"特定器材名称", {maybe, unicode}, 254} %% 漢字 254 可変
          %% 1 未コード化特定器材の場合は、告示名を記録する。
          %% 2 特定器材名称が254バイトに満たない場合は、後続する スペース を省略しても差し支えない。
          %% 3 その他の場合は、記録を省略する。
        ]).

