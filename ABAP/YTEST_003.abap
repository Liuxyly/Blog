*----------------------------------------------------------------------*
* Report: ZIC100701 /入金額不一致リスト
* RICEF : IC1007
*----------------------------------------------------------------------*
* Description:
* 再計算等の理由により、お客さまに対する請求の金額と振込票で
* 入金された金額が不一致となった場合、仮受金が計上される。当機能は、
* 仮受金で計上された未消込のFI-CA伝票を一覧で表示する機能である。
*----------------------------------------------------------------------*
* 001 2015/07/01  JEDK900324 SOACN064
* 新規作成
* 002 2015/10/29  JEDK903399 SOACN064
* SIR#944
*     計算エラーお客さま一覧：料金計算エラー一覧画面からダウンロードした、
*     でも、テーブル「ZAUCST0003」に実行者のデータが格納されません
*----------------------------------------------------------------------*

REPORT ZIC100701.

*----------------------------------------------------------------------*
*  Type Definition
*----------------------------------------------------------------------*
TYPES:
  BEGIN OF GTS_DFKKOP,
    OPBEL TYPE DFKKOP-OPBEL,                                 "伝票番号
    OPUPZ TYPE DFKKOP-OPUPZ,                                 "補助明細番号
    GPART TYPE DFKKOP-GPART,                                 "ビジネスパートナ
    VKONT TYPE DFKKOP-VKONT,                                 "契約アカウント
    BLDAT TYPE DFKKOP-BLDAT,                                 "伝票日付
    BUDAT TYPE DFKKOP-BUDAT,                                 "転記日付
    WAERS TYPE DFKKOP-WAERS,                                 "取引通貨
    BETRW TYPE DFKKOP-BETRW,                                 "金額
    HERKF TYPE TFK001T-HERKF,                                "発生源キー
    HTEXT TYPE TFK001T-HTEXT,                                "発生源テキスト
  END OF GTS_DFKKOP,
*> INS SIR#944 2015/10/29 JEDK903399-----------------------------------*
  BEGIN OF GTS_LOG,
    GPART TYPE DFKKOP-GPART,                                 "ビジネスパートナ
    VKONT TYPE DFKKOP-VKONT,                                 "契約アカウント
    OPBEL TYPE DFKKOP-OPBEL,                                 "伝票番号
    OPUPZ TYPE DFKKOP-OPUPZ,                                 "補助明細番号
    BUDAT TYPE DFKKOP-BUDAT,                                 "転記日付
    BLDAT TYPE DFKKOP-BLDAT,                                 "伝票日付
    BETRW TYPE DFKKOP-BETRW,                                 "金額
    HERKF TYPE TFK001T-HERKF,                                "発生源キー
    HTEXT TYPE TFK001T-HTEXT,                                "発生源テキスト
  END OF GTS_LOG,
  GTT_LOG TYPE STANDARD TABLE OF GTS_LOG,                    "ログのテーブル型
*< INS SIR#944 2015/10/29 JEDK903399-----------------------------------*
  GTT_DFKKOP TYPE STANDARD TABLE OF GTS_DFKKOP,              "契約アカウント伝票の明細のテーブル型

  GTT_ALV TYPE STANDARD TABLE OF ZICS0017,                   "ALV出力のテーブル型
  GTT_R_UKD    TYPE RANGE OF DFKKOP-BUDAT.                   "転記日付レンジ型

*----------------------------------------------------------------------*
*  Constant definition
*----------------------------------------------------------------------*
CONSTANTS:
*> INS SIR#944 2015/10/29 JEDK903399-----------------------------------*
  GCF_DL_PARAID   TYPE CHAR8      VALUE 'ZAUCA001',           "一覧ＤＬログ対象のパラメータ値
  GCF_DL_PARAM    TYPE CHAR3      VALUE '%PC',
*< INS SIR#944 2015/10/29 JEDK903399-----------------------------------*
  GCF_CODE        TYPE CHAR30     VALUE 'WAERS',              "通貨コード
  GCF_OBJ         TYPE BALOBJ_D   VALUE 'ZIC000',             "オブジェクト
  GCF_SUBOBJ      TYPE BALSUBOBJ  VALUE 'ZIC1007',            "サブオブジェクト
  GCF_PFSTATUS    TYPE SYPFKEY    VALUE 'Z001',               "ステータス
  GCF_MSGTYP_SUC  TYPE CHAR1      VALUE 'S',                  "メッセージタイプ(成功)
  GCF_MSGTYP_ERR  TYPE CHAR1      VALUE 'E',                  "メッセージタイプ(エラー)
  GCF_HVORG       TYPE CHAR4      VALUE '0060',               "入金(未決済)
  GCF_TVORG1      TYPE CHAR4      VALUE '0010',               "仮受金
  GCF_TVORG2      TYPE CHAR4      VALUE '0020',               "仮受金（リセット）
  GCF_HERKF1      TYPE CHAR2      VALUE '05',                 "入金ロット
  GCF_HERKF2      TYPE CHAR2      VALUE '09',                 "リセット消込明細
  GCF_SPRAS       TYPE CHAR2      VALUE 'JA'.                 "日本語

*----------------------------------------------------------------------*
*  Data definition
*----------------------------------------------------------------------*
DATA:
*   ALVソート用データ
    BEGIN OF GDS_ALVSORT,
      GPART       TYPE SCRTEXT_L,                            "ビジネスパートナ
      VKONT       TYPE SCRTEXT_L,                            "契約アカウント
      OPBEL       TYPE SCRTEXT_L,                            "伝票番号
    END OF GDS_ALVSORT,
    GDR_ALV_TABLE TYPE REF TO CL_SALV_TABLE,                 "ALVテーブルクラス
    GDF_BUDAT     TYPE DFKKOP-BUDAT,                         "転記日付
    GDT_DFKKOP    TYPE GTT_DFKKOP,                           "契約アカウント伝票の明細のテーブル
    GDF_ERR_FLG   TYPE FLAG,                                 "エラーフラグ
    GDT_AVL       TYPE GTT_ALV.                              "ALV出力のテーブル

*> INS SIR#944 2015/10/29 JEDK903399-----------------------------------*
*----------------------------------------------------------------------*
*       CLASS LCL_ALV_HANDLE_EVENTS DEFINITION
*----------------------------------------------------------------------*
*       ALVのイベント
*----------------------------------------------------------------------*
CLASS LCL_ALV_HANDLE_EVENTS DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      ON_BEFORE_SALV_FUNCTION FOR EVENT BEFORE_SALV_FUNCTION
                                     OF CL_SALV_EVENTS_TABLE
                              IMPORTING E_SALV_FUNCTION,
      ON_AFTER_SALV_FUNCTION  FOR EVENT AFTER_SALV_FUNCTION
                                     OF CL_SALV_EVENTS_TABLE
                              IMPORTING E_SALV_FUNCTION.

ENDCLASS.                    "LCL_ALV_HANDLE_EVENTS DEFINITION
*< INS SIR#944 2015/10/29 JEDK903399-----------------------------------*

*&---------------------------------------------------------------------*
*&  選択画面
*&---------------------------------------------------------------------*
* 処理対象
SELECTION-SCREEN BEGIN OF BLOCK BK1 WITH FRAME TITLE TEXT-S01.
  SELECT-OPTIONS: S_BUDAT FOR GDF_BUDAT.                     "転記日付
SELECTION-SCREEN END OF BLOCK BK1.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.

* 初期処理
  PERFORM INIT_PROC.

*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.

* 契約アカウント伝票の明細を取得
  PERFORM DB_GET_DATA_DFKKOP
    USING
      S_BUDAT[]                     "選択画面の転記日付
    CHANGING
      GDT_DFKKOP                    "契約アカウント伝票の明細のテーブル
      GDF_ERR_FLG.                  "エラーフラグ

* 契約アカウント伝票の明細を取得する失敗の場合
  IF GDF_ERR_FLG = ABAP_TRUE.
*   処理なし
* 契約アカウント伝票の明細を取得する成功の場合
  ELSE.

*   契約アカウント伝票の明細を編集
    PERFORM EDIT_DFKKOP
      USING
        GDT_DFKKOP                  "契約アカウント伝票の明細のテーブル
      CHANGING
        GDT_AVL.                    "ALV出力のテーブル

*   ALV出力
    PERFORM ALV_DISPLAY
      USING
        GDT_AVL.                    "ALV出力のテーブル
  ENDIF.

*----------------------------------------------------------------------*
* END-OF-SELECTION.
*----------------------------------------------------------------------*
END-OF-SELECTION.

* アプリログ出力
  ZAUCL_CA_MESSAGE_MANAGER=>OUTPUT_CONTROL_LOG(
    EXPORTING
      PVF_I_APPLOG_OBJECT = GCF_OBJ
      PVF_I_APPLOG_SUBOBJ = GCF_SUBOBJ ).


*&---------------------------------------------------------------------*
*&      Form  DB_GET_DATA_DFKKOP
*&---------------------------------------------------------------------*
*       契約アカウント伝票の明細を取得
*----------------------------------------------------------------------*
*      -->PRT_I_R_BUDAT  選択画面の転記日付
*      <--PRT_O_DFKKOP   契約アカウント伝票の明細のテーブル
*      <--PRF_O_ERR_FLG  エラーフラグ
*----------------------------------------------------------------------*
FORM DB_GET_DATA_DFKKOP
  USING
    PRT_I_R_BUDAT   TYPE GTT_R_UKD
  CHANGING
    PRT_O_DFKKOP    TYPE GTT_DFKKOP
    PRF_O_ERR_FLG   TYPE C.

  DATA:
    LDF_MSGTXT TYPE STRING.                                  "メッセージ出力用

  SELECT DFKKOP~OPBEL                                        "伝票番号
         DFKKOP~OPUPZ                                        "補助明細番号
         DFKKOP~GPART                                        "ビジネスパートナ
         DFKKOP~VKONT                                        "契約アカウント
         DFKKOP~BLDAT                                        "伝票日付
         DFKKOP~BUDAT                                        "転記日付
         DFKKOP~WAERS                                        "取引通貨
         DFKKOP~BETRW                                        "金額
         TFK001T~HERKF                                       "発生源キー
         TFK001T~HTEXT                                       "発生源テキスト
   INTO TABLE PRT_O_DFKKOP
   FROM DFKKOP
  INNER JOIN DFKKKO
     ON DFKKOP~OPBEL    =  DFKKKO~OPBEL
  INNER JOIN TFK001T
     ON DFKKKO~HERKF    =  TFK001T~HERKF
  WHERE DFKKOP~AUGST    =  ABAP_FALSE                        "未消込明細(固定値：ブランク)
    AND DFKKOP~HVORG    =  GCF_HVORG                         "入金(未決済)(固定値：'0060')
    AND ( DFKKOP~TVORG  =  GCF_TVORG1                        "仮受金(固定値：'0010')
     OR   DFKKOP~TVORG  =  GCF_TVORG2 )                      "仮受金(固定値：'0020')
    AND ( DFKKKO~HERKF  =  GCF_HERKF1                        "入金ロット(固定値：'05')
     OR DFKKKO~HERKF    =  GCF_HERKF2 )                      "リセット消込明細(固定値：'09')
    AND DFKKKO~BUDAT    IN PRT_I_R_BUDAT                     "選択画面の転記日付
    AND TFK001T~SPRAS   =  GCF_SPRAS.                        "日本語(固定値：'JA')

* 取得の場合
  IF SY-SUBRC = 0.
*   処理なし
* 取得できない場合
  ELSE.

    PRF_O_ERR_FLG = ABAP_TRUE.                               "処理終了フラグ設定

*   エラーメッセージ「対象データがありません」
    MESSAGE S001(ZIC000) INTO LDF_MSGTXT.

    CALL METHOD ZAUCL_CA_MESSAGE_MANAGER=>OUTPUT_MESSAGE( ).

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  INIT_PROC
*&---------------------------------------------------------------------*
*       初期処理
*----------------------------------------------------------------------*
FORM INIT_PROC.

  CLEAR:
    GDT_DFKKOP,
    GDF_ERR_FLG,
    GDT_AVL.
* アプリケーションログセット
  ZAUCL_CA_MESSAGE_MANAGER=>SET_LOG_OBJECT( PVF_I_APPLOG_OBJECT = GCF_OBJ
                                            PVF_I_APPLOG_SUBOBJ = GCF_SUBOBJ ).
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  EDIT_DFKKOP
*&---------------------------------------------------------------------*
*       契約アカウント伝票の明細を編集
*----------------------------------------------------------------------*
*      -->PRT_I_DFKKOP  契約アカウント伝票の明細のテーブル
*      <--PRT_O_AVL     ALV出力のテーブル
*----------------------------------------------------------------------*
FORM EDIT_DFKKOP
  USING
    PRT_I_DFKKOP TYPE GTT_DFKKOP
  CHANGING
    PRT_O_AVL    TYPE GTT_ALV.

  FIELD-SYMBOLS:
    <LFS_DFKKOP> TYPE GTS_DFKKOP.                       "契約アカウント伝票の明細のフィールドシンボル

  DATA:
    LDS_ALV      TYPE ZICS0017.                         "ALV出力の結構

  LOOP AT PRT_I_DFKKOP ASSIGNING <LFS_DFKKOP>.

    CLEAR:
      LDS_ALV.

    LDS_ALV-GPART = <LFS_DFKKOP>-GPART.                 "ビジネスパートナ
    LDS_ALV-VKONT = <LFS_DFKKOP>-VKONT.                 "契約アカウント
    LDS_ALV-OPBEL = <LFS_DFKKOP>-OPBEL.                 "伝票番号
    LDS_ALV-OPUPZ = <LFS_DFKKOP>-OPUPZ.                 "補助明細番号
    LDS_ALV-BUDAT = <LFS_DFKKOP>-BUDAT.                 "転記日付
    LDS_ALV-BLDAT = <LFS_DFKKOP>-BLDAT.                 "伝票日付
    LDS_ALV-BETRW = <LFS_DFKKOP>-BETRW.                 "金額
    LDS_ALV-HERKF = <LFS_DFKKOP>-HERKF.                 "発生源キー
    LDS_ALV-HTEXT = <LFS_DFKKOP>-HTEXT.                 "発生源テキスト
    LDS_ALV-WAERS = <LFS_DFKKOP>-WAERS.                 "取引通貨

    APPEND LDS_ALV TO PRT_O_AVL.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ALV_DISPLAY
*&---------------------------------------------------------------------*
*       ALV出力
*----------------------------------------------------------------------*
*      -->P_GDT_AVL  ALV出力のテーブル
*----------------------------------------------------------------------*
FORM ALV_DISPLAY
  USING
    PRT_I_ALV TYPE GTT_ALV.

  DATA:
*> INS SIR#944 2015/10/29 JEDK903399-----------------------------------*
    LDR_ALV_EVENTS_TABLE   TYPE REF TO CL_SALV_EVENTS_TABLE,       "ALVイベント
*< INS SIR#944 2015/10/29 JEDK903399-----------------------------------*
    LDF_ERR_FLAG TYPE FLAG.    "エラーフラグ

* ALVインスタンスを生成
  PERFORM ALV_INSTANT
    CHANGING
      PRT_I_ALV                "ALV出力のテーブル
      LDF_ERR_FLAG.            "エラーフラグ

* ALVインスタンスを生成する失敗の場合
  IF LDF_ERR_FLAG = ABAP_TRUE.
*   処理なし
* ALVインスタンスを生成する成功の場合
  ELSE.
*   ALVの機能設定
    PERFORM ALV_FUNCTION.

*   ALVのレイアウト設定
    PERFORM ALV_LAYOUT.

*   ALVの選択設定
    PERFORM ALV_SELECTION.

*   ALVの表示設定
    PERFORM ALV_DISPLAY_SETTINGS.

*   ALVの列を設定
    PERFORM ALV_COLUMNS_TABLE
      CHANGING
        LDF_ERR_FLAG.
  ENDIF.

* ALVインスタンスを生成する失敗の場合
  IF LDF_ERR_FLAG = ABAP_TRUE.
*   処理なし
* ALVインスタンスを生成する成功の場合
  ELSE.
*   ALVのソートを設定
    PERFORM ALV_SORT_DATA
      USING
        GDS_ALVSORT            "ALV表示用データ
      CHANGING
        LDF_ERR_FLAG.          "エラーフラグ
*> INS SIR#944 2015/10/29 JEDK903399------------------------------------*
*   ALVイベント設定
    LDR_ALV_EVENTS_TABLE = GDR_ALV_TABLE->GET_EVENT( ).
    SET HANDLER LCL_ALV_HANDLE_EVENTS=>ON_BEFORE_SALV_FUNCTION FOR LDR_ALV_EVENTS_TABLE.
    SET HANDLER LCL_ALV_HANDLE_EVENTS=>ON_AFTER_SALV_FUNCTION FOR LDR_ALV_EVENTS_TABLE.
*< INS SIR#944 2015/10/29 JEDK903399------------------------------------*
  ENDIF.
* ALVのソートを設定する失敗の場合
  IF LDF_ERR_FLAG = ABAP_TRUE.
*   処理なし
* ALVのソートを設定する成功の場合
  ELSE.
*   ALVを表示
    GDR_ALV_TABLE->DISPLAY( ).
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ALV_INSTANT
*&---------------------------------------------------------------------*
*       ALVインスタンスを生成
*----------------------------------------------------------------------*
*      -->PRT_I_ALV      ALV出力のテーブル
*      -->PRF_O_ERR_FLG  フラグ
*----------------------------------------------------------------------*
FORM ALV_INSTANT
  CHANGING
    PRT_O_ALV     TYPE GTT_ALV
    PRF_O_ERR_FLG TYPE FLAG.

  DATA:
    LDT_ROOT   TYPE REF TO CX_ROOT,          "例外のクラス
    LDF_MSGTXT TYPE STRING.                  "メッセージテキスト

  TRY.
    CL_SALV_TABLE=>FACTORY(
      IMPORTING
        R_SALV_TABLE = GDR_ALV_TABLE
      CHANGING
        T_TABLE      = PRT_O_ALV[] ).

  CATCH CX_ROOT INTO LDT_ROOT.
    MESSAGE E080(ZIC000) WITH TEXT-M01 TEXT-M02 SPACE SPACE
       INTO LDF_MSGTXT.
*   処理結果設定共通処理
    ZAUCL_CA_MESSAGE_MANAGER=>OUTPUT_MESSAGE( PVF_I_REPLACE_TYPE = GCF_MSGTYP_SUC
                                              PVF_I_DISPLAY_LIKE = GCF_MSGTYP_ERR ).
    PRF_O_ERR_FLG = ABAP_TRUE.
    RETURN.
  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ALV_FUNCTION
*&---------------------------------------------------------------------*
*       ALVの機能設定
*----------------------------------------------------------------------*
FORM ALV_FUNCTION.
  DATA:
    LDR_ALV_ALV_FUNCTIONS_LIST TYPE REF TO CL_SALV_FUNCTIONS_LIST.   "機能リスト

*   ALVの機能設定
    GDR_ALV_TABLE->SET_SCREEN_STATUS(
      PFSTATUS      = GCF_PFSTATUS
      REPORT        = SY-REPID
      SET_FUNCTIONS = GDR_ALV_TABLE->C_FUNCTIONS_ALL ).
    LDR_ALV_ALV_FUNCTIONS_LIST = GDR_ALV_TABLE->GET_FUNCTIONS( ).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ALV_LAYOUT
*&---------------------------------------------------------------------*
*       ALVのレイアウト設定
*----------------------------------------------------------------------*
FORM ALV_LAYOUT .
  DATA:
    LDR_ALV_LAYOUT     TYPE REF TO CL_SALV_LAYOUT,       "ALVレイアウト設定
    LDS_ALV_LAYOUT_KEY TYPE SALV_S_LAYOUT_KEY.           "ALVレイアウトキー

  LDR_ALV_LAYOUT = GDR_ALV_TABLE->GET_LAYOUT( ).
  LDS_ALV_LAYOUT_KEY-REPORT = SY-REPID.
  LDR_ALV_LAYOUT->SET_KEY( LDS_ALV_LAYOUT_KEY ).
  LDR_ALV_LAYOUT->SET_SAVE_RESTRICTION( IF_SALV_C_LAYOUT=>RESTRICT_NONE ).
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ALV_SELECTION
*&---------------------------------------------------------------------*
*       ALVの選択設定
*----------------------------------------------------------------------*
FORM ALV_SELECTION .
  DATA:
    LDR_ALV_SELECTIONS   TYPE REF TO CL_SALV_SELECTIONS. "ALV選択設定

  LDR_ALV_SELECTIONS = GDR_ALV_TABLE->GET_SELECTIONS( ).
  LDR_ALV_SELECTIONS->SET_SELECTION_MODE(
    IF_SALV_C_SELECTION_MODE=>ROW_COLUMN ).
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ALV_SORT_DATA
*&---------------------------------------------------------------------*
*       ALVのソートを設定
*----------------------------------------------------------------------*
*      <--PRT_I_ALV      ALV表示用データ
*      -->PRF_O_ERR_FLG  フラグ
*----------------------------------------------------------------------*
FORM ALV_SORT_DATA
  USING
    PRT_I_ALV     LIKE GDS_ALVSORT
  CHANGING
    PRF_O_ERR_FLG TYPE FLAG.

  DATA:
    LDR_REF          TYPE REF TO CL_ABAP_STRUCTDESCR,    "実行時タイプサービス
    LDF_POSITION     TYPE I,                             "ポジション
    LDT_ROOT         TYPE REF TO CX_ROOT,                "例外のクラス
    LDF_MSGTXT       TYPE STRING,                        "メッセージテキスト
    LDR_ALV_SORTS    TYPE REF TO CL_SALV_SORTS.          "ALVソート

  FIELD-SYMBOLS:
    <LFS_ALV>        TYPE ABAP_COMPDESCR.

* ALVのソートを設定
  LDR_ALV_SORTS = GDR_ALV_TABLE->GET_SORTS( ).

  TRY.
    LDR_REF ?= CL_ABAP_TYPEDESCR=>DESCRIBE_BY_DATA( PRT_I_ALV ).
    LOOP AT LDR_REF->COMPONENTS ASSIGNING <LFS_ALV>.
      LDF_POSITION = LDF_POSITION + 1.
      LDR_ALV_SORTS->ADD_SORT(
        COLUMNNAME = <LFS_ALV>-NAME                      "ソート項目: データ更新日
        POSITION   = LDF_POSITION                        "位置
        SEQUENCE   = IF_SALV_C_SORT=>SORT_UP             "ソート順: 昇順
        SUBTOTAL   = IF_SALV_C_BOOL_SAP=>FALSE           "小計ブレイクキー: 無効
        GROUP      = IF_SALV_C_SORT=>GROUP_NONE          "グループ: なし
        OBLIGATORY = IF_SALV_C_BOOL_SAP=>FALSE ).        "必須: 無効
    ENDLOOP.
  CATCH CX_ROOT INTO LDT_ROOT.
    MESSAGE E080(ZIC000) WITH TEXT-M01 TEXT-M02 SPACE SPACE
       INTO LDF_MSGTXT.
*   処理結果設定共通処理
    ZAUCL_CA_MESSAGE_MANAGER=>OUTPUT_MESSAGE( PVF_I_REPLACE_TYPE = GCF_MSGTYP_SUC
                                              PVF_I_DISPLAY_LIKE = GCF_MSGTYP_ERR ).
    PRF_O_ERR_FLG = ABAP_TRUE.
    RETURN.
  ENDTRY.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ALV_DISPLAY_SETTINGS
*&---------------------------------------------------------------------*
*       ALVの表示設定
*----------------------------------------------------------------------*
FORM ALV_DISPLAY_SETTINGS.
  DATA:
    LDR_ALV_DISPLAY_SETTINGS TYPE REF TO CL_SALV_DISPLAY_SETTINGS.   "ALV表示設定

  LDR_ALV_DISPLAY_SETTINGS = GDR_ALV_TABLE->GET_DISPLAY_SETTINGS( ).
  LDR_ALV_DISPLAY_SETTINGS->SET_STRIPED_PATTERN( 'X' ).              "ゼブラ表示
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ALV_COLUMNS_TABLE
*&---------------------------------------------------------------------*
*       ALVの列を設定
*----------------------------------------------------------------------*
*      <--PRF_O_ERR_FLG     エラーフラグ
*----------------------------------------------------------------------*
FORM ALV_COLUMNS_TABLE
  CHANGING
    PRF_O_ERR_FLG TYPE FLAG.

  DATA:
    LDT_ROOT              TYPE REF TO CX_ROOT,                       "例外のクラス
    LDF_MSGTXT            TYPE STRING,                               "メッセージテキスト
    LDR_ALV_COLUMN        TYPE REF TO CL_SALV_COLUMN,                "コラム
    LDR_ALV_COLUMNS_TABLE TYPE REF TO CL_SALV_COLUMNS_TABLE.         "ALV列

  LDR_ALV_COLUMNS_TABLE = GDR_ALV_TABLE->GET_COLUMNS( ).
  LDR_ALV_COLUMNS_TABLE->SET_OPTIMIZE( IF_SALV_C_BOOL_SAP=>TRUE ).   "表示幅を最適化

  TRY.
*   通貨コーダを表示しない
    LDR_ALV_COLUMN = LDR_ALV_COLUMNS_TABLE->GET_COLUMN(  GCF_CODE ).
    LDR_ALV_COLUMN->SET_VISIBLE( IF_SALV_C_BOOL_SAP=>FALSE ).
  CATCH CX_ROOT INTO LDT_ROOT.
    MESSAGE E002(ZCA000) WITH TEXT-M01 TEXT-M02 SPACE SPACE
       INTO LDF_MSGTXT.
*   処理結果設定共通処理
    ZAUCL_CA_MESSAGE_MANAGER=>OUTPUT_MESSAGE( PVF_I_REPLACE_TYPE = GCF_MSGTYP_SUC
                                              PVF_I_DISPLAY_LIKE = GCF_MSGTYP_ERR ).
    PRF_O_ERR_FLG = ABAP_TRUE.
    RETURN.
  ENDTRY.
ENDFORM.
*> INS SIR#944 2015/10/29 JEDK903399-----------------------------------*
*&---------------------------------------------------------------------*
*&      Form  EDIT_LOG
*&---------------------------------------------------------------------*
*       ログ用を編集
*----------------------------------------------------------------------*
*      -->PRT_I_AVL     ALV出力のテーブル
*      <--PRT_O_LOG     ログ用のテーブル
*----------------------------------------------------------------------*
FORM EDIT_LOG
  USING
    PRT_I_AVL    TYPE GTT_ALV
  CHANGING
    PRT_O_LOG    TYPE GTT_LOG.

  FIELD-SYMBOLS:
    <LFS_ALV>    TYPE ZICS0017.                         "ALV用のフィールドシンボル

  DATA:
    LDS_LOG      TYPE GTS_LOG.                          "ログ用の結構

  LOOP AT PRT_I_AVL ASSIGNING <LFS_ALV>.

    CLEAR:
      LDS_LOG.

    LDS_LOG-GPART = <LFS_ALV>-GPART.                    "ビジネスパートナ
    LDS_LOG-VKONT = <LFS_ALV>-VKONT.                    "契約アカウント
    LDS_LOG-OPBEL = <LFS_ALV>-OPBEL.                    "伝票番号
    LDS_LOG-OPUPZ = <LFS_ALV>-OPUPZ.                    "補助明細番号
    LDS_LOG-BUDAT = <LFS_ALV>-BUDAT.                    "転記日付
    LDS_LOG-BLDAT = <LFS_ALV>-BLDAT.                    "伝票日付
    LDS_LOG-BETRW = <LFS_ALV>-BETRW.                    "金額
    LDS_LOG-HERKF = <LFS_ALV>-HERKF.                    "発生源キー
    LDS_LOG-HTEXT = <LFS_ALV>-HTEXT.                    "発生源テキスト

    APPEND LDS_LOG TO PRT_O_LOG.
  ENDLOOP.
ENDFORM.
*----------------------------------------------------------------------*
*       CLASS LCL_ALV_HANDLE_EVENTS IMPLEMENTATION
*----------------------------------------------------------------------*
*       ALVのイベント
*----------------------------------------------------------------------*
CLASS LCL_ALV_HANDLE_EVENTS IMPLEMENTATION.

  METHOD ON_BEFORE_SALV_FUNCTION.

    CASE E_SALV_FUNCTION.
      WHEN GCF_DL_PARAM.              "ローカルファイルへのエクスポート
        SET PARAMETER ID GCF_DL_PARAID FIELD ABAP_FALSE.
      WHEN OTHERS.
*       処理なし
    ENDCASE.

  ENDMETHOD.                    "ON_BEFORE_SALV_FUNCTION

  METHOD ON_AFTER_SALV_FUNCTION.

    DATA:
      LDT_LOG       TYPE GTT_LOG,     "ログのテーブル
      LDF_IS_ACTIVE TYPE XFELD.       "チェックボックス

    CASE E_SALV_FUNCTION.
      WHEN GCF_DL_PARAM.              "ローカルファイルへのエクスポート
        GET PARAMETER ID GCF_DL_PARAID FIELD LDF_IS_ACTIVE.
        IF LDF_IS_ACTIVE = ABAP_TRUE.
*         ログ用を編集
          PERFORM EDIT_LOG
            USING
              GDT_AVL[]               "ALV出力のテーブル
            CHANGING
              LDT_LOG[].              "ログ用のテーブル

          CALL METHOD ZAUCL_CA_DL_LOG=>WRITE_AUDIT_LOG
              EXPORTING
                 PVF_I_CPROG   = SY-CPROG
                 PVT_I_LOGDATA = LDT_LOG[].
        ENDIF.

        SET PARAMETER ID GCF_DL_PARAID FIELD ABAP_FALSE.

      WHEN OTHERS.
*       処理なし
    ENDCASE.

  ENDMETHOD.                    "ON_AFTER_SALV_FUNCTION

ENDCLASS.                    "LCL_ALV_HANDLE_EVENTS IMPLEMENTATION
*< INS SIR#944 2015/10/29 JEDK903399-----------------------------------*
