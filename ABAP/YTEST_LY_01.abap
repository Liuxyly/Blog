*&---------------------------------------------------------------------*
*& Report  YALVOO_SAMPLE
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  YTEST_LY_01.

TABLES:
  SFLIGHT.

*----------------------------------------------------------------------*
*       CLASS LCL_ALV_HANDLE_EVENTS DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_ALV_HANDLE_EVENTS DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:

      ON_BEFORE_SALV_FUNCTION FOR EVENT BEFORE_SALV_FUNCTION OF CL_SALV_EVENTS_TABLE
        IMPORTING E_SALV_FUNCTION,

      ON_AFTER_SALV_FUNCTION  FOR EVENT AFTER_SALV_FUNCTION  OF CL_SALV_EVENTS_TABLE
        IMPORTING E_SALV_FUNCTION,

      ON_USER_COMMAND         FOR EVENT ADDED_FUNCTION       OF CL_SALV_EVENTS_TABLE
        IMPORTING E_SALV_FUNCTION,

      ON_DOUBLE_CLICK         FOR EVENT DOUBLE_CLICK         OF CL_SALV_EVENTS_TABLE
        IMPORTING ROW COLUMN,

      ON_LINK_CLICK           FOR EVENT LINK_CLICK           OF CL_SALV_EVENTS_TABLE
        IMPORTING ROW COLUMN.
ENDCLASS.                    "lcl_alv_handle_events DEFINITION

* ALV出力テーブル
DATA:
  GDT_ALV_DATA    TYPE STANDARD TABLE OF SFLIGHT,
*  GDS_ALV_DATA    LIKE LINE OF GDT_ALV_DATA[],
  GDR_ALV_TABLE   TYPE REF TO CL_SALV_TABLE,
  GDR_ALV_EVENTS  TYPE REF TO LCL_ALV_HANDLE_EVENTS,
  GDF_ALV_LINES   TYPE I.                               "ALVエントリ数

SELECT-OPTIONS:
  S_FLDATE FOR SFLIGHT-FLDATE NO-EXTENSION.

START-OF-SELECTION.

  SELECT * FROM SFLIGHT INTO TABLE GDT_ALV_DATA[]
    WHERE FLDATE IN S_FLDATE[].

END-OF-SELECTION.

  PERFORM F_ALV_WRITE.

*&---------------------------------------------------------------------*
*&      Form  F_ALV_WRITE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_ALV_WRITE.

* ALV制御用データ
  CONSTANTS:
    LCF_ALV_ENABLE_DETAIL      TYPE BOOLEAN VALUE ' ',
    LCF_ALV_ENABLE_DOWNLOADS   TYPE BOOLEAN VALUE 'X'.

  DATA:
    LDR_ALV_DISPLAY_SETTINGS   TYPE REF TO CL_SALV_DISPLAY_SETTINGS, "ALV表示設定
    LDR_ALV_LAYOUT             TYPE REF TO CL_SALV_LAYOUT,           "ALVレイアウト設定
    LDR_ALV_SELECTIONS         TYPE REF TO CL_SALV_SELECTIONS,       "ALV選択設定
    LDR_ALV_FUNCTIONS          TYPE REF TO CL_SALV_FUNCTIONS,        "ALV機能設定
    LDR_ALV_ALV_FUNCTIONS_LIST TYPE REF TO CL_SALV_FUNCTIONS_LIST,
    LDR_ALV_COLUMNS_TABLE      TYPE REF TO CL_SALV_COLUMNS_TABLE,    "ALV列
    LDR_ALV_COLUMN_LIST        TYPE REF TO CL_SALV_COLUMN_LIST,
    LDR_ALV_COLUMN             TYPE REF TO CL_SALV_COLUMN,
    LDR_ALV_SORTS              TYPE REF TO CL_SALV_SORTS,            "ALVソート
    LDR_ALV_AGGREGATIONS       TYPE REF TO CL_SALV_AGGREGATIONS,     "ALV集約設定
    LDR_ALV_FORM_ELEMENT       TYPE REF TO CL_SALV_FORM_ELEMENT,     "
    LDR_ALV_EVENTS_TABLE       TYPE REF TO CL_SALV_EVENTS_TABLE,     "ALVイベント
    LDF_ALV_TITLE              TYPE LVC_TITLE,                       "ALV表題
    LDS_ALV_LAYOUT_KEY         TYPE SALV_S_LAYOUT_KEY.               "ALVレイアウトキー

* ALV出力件数を算出
  GDF_ALV_LINES = LINES( GDT_ALV_DATA[] ).

* ALVインスタンスを生成
  TRY.
      CL_SALV_TABLE=>FACTORY(
        IMPORTING
          R_SALV_TABLE = GDR_ALV_TABLE
        CHANGING
          T_TABLE      = GDT_ALV_DATA[] ).
    CATCH CX_SALV_MSG.
      ASSERT 0 = 1.
  ENDTRY.

* ALVの表示設定
  LDR_ALV_DISPLAY_SETTINGS = GDR_ALV_TABLE->GET_DISPLAY_SETTINGS( ).
  LDR_ALV_DISPLAY_SETTINGS->SET_STRIPED_PATTERN( 'X' ).       "ゼブラ表示
  LDF_ALV_TITLE = SY-TITLE.                                   "レポート表題
  LDR_ALV_DISPLAY_SETTINGS->SET_LIST_HEADER( LDF_ALV_TITLE ). "表題を設定

* ALVの機能設定
  GDR_ALV_TABLE->SET_SCREEN_STATUS(
    PFSTATUS      = 'ZTEST01'
    REPORT        = SY-REPID
    SET_FUNCTIONS = GDR_ALV_TABLE->C_FUNCTIONS_DEFAULT ).
  LDR_ALV_ALV_FUNCTIONS_LIST = GDR_ALV_TABLE->GET_FUNCTIONS( ).

  IF LCF_ALV_ENABLE_DETAIL = 'X'.    "詳細を有効化
    LDR_ALV_ALV_FUNCTIONS_LIST->SET_DETAIL( IF_SALV_C_BOOL_SAP=>TRUE ).
  ENDIF.
  IF LCF_ALV_ENABLE_DOWNLOADS = 'X'. "ローカルファイルへのエクスポートを有効化
    LDR_ALV_ALV_FUNCTIONS_LIST->SET_EXPORT_LOCALFILE( IF_SALV_C_BOOL_SAP=>TRUE ).
  ENDIF.

* ALVのレイアウト設定
  LDR_ALV_LAYOUT = GDR_ALV_TABLE->GET_LAYOUT( ).
  LDS_ALV_LAYOUT_KEY-REPORT = SY-REPID.
  LDR_ALV_LAYOUT->SET_KEY( LDS_ALV_LAYOUT_KEY ).
  LDR_ALV_LAYOUT->SET_SAVE_RESTRICTION( IF_SALV_C_LAYOUT=>RESTRICT_NONE ).

* ALVの選択設定
  LDR_ALV_SELECTIONS = GDR_ALV_TABLE->GET_SELECTIONS( ).
  LDR_ALV_SELECTIONS->SET_SELECTION_MODE( IF_SALV_C_SELECTION_MODE=>ROW_COLUMN ).
*
* ALVの列を設定
  LDR_ALV_COLUMNS_TABLE = GDR_ALV_TABLE->GET_COLUMNS( ).
  LDR_ALV_COLUMNS_TABLE->SET_OPTIMIZE( IF_SALV_C_BOOL_SAP=>TRUE ).     "表示幅を最適化

* ALVのキー列を設定
  LDR_ALV_COLUMNS_TABLE->SET_KEY_FIXATION( IF_SALV_C_BOOL_SAP=>TRUE ). "キー列を固定

* ALVの特定列を非表示
  "クライアントを非表示
  TRY.
      LDR_ALV_COLUMN = LDR_ALV_COLUMNS_TABLE->GET_COLUMN( 'MANDT' ).
      LDR_ALV_COLUMN->SET_VISIBLE( IF_SALV_C_BOOL_SAP=>FALSE ).
    CATCH CX_SALV_NOT_FOUND.
  ENDTRY.
*
* ALVの特例列にホットスポットを設定
  TRY.
      LDR_ALV_COLUMN_LIST ?= LDR_ALV_COLUMNS_TABLE->GET_COLUMN( 'PLANETYPE' ).
      LDR_ALV_COLUMN_LIST->SET_CELL_TYPE( IF_SALV_C_CELL_TYPE=>HOTSPOT ).
  CATCH CX_SALV_NOT_FOUND.
  ENDTRY.

* ALVのソートを設定
  LDR_ALV_SORTS = GDR_ALV_TABLE->GET_SORTS( ).
  TRY.
      LDR_ALV_SORTS->ADD_SORT(
        COLUMNNAME = 'CARRID'                     "ソート項目: 航空会社コード
        POSITION   = 1                            "位置
        SEQUENCE   = IF_SALV_C_SORT=>SORT_UP      "ソート順: 昇順
        SUBTOTAL   = IF_SALV_C_BOOL_SAP=>TRUE    "小計ブレイクキー: 無効
        GROUP      = IF_SALV_C_SORT=>GROUP_NONE   "グループ: なし
        OBLIGATORY = IF_SALV_C_BOOL_SAP=>FALSE ). "必須: 無効
      LDR_ALV_SORTS->ADD_SORT(
        COLUMNNAME = 'CONNID'                     "ソート項目: フライト接続番号
        POSITION   = 2                            "位置
        SEQUENCE   = IF_SALV_C_SORT=>SORT_UP      "ソート順: 昇順
        SUBTOTAL   = IF_SALV_C_BOOL_SAP=>TRUE     "小計ブレイクキー: 有効
        GROUP      = IF_SALV_C_SORT=>GROUP_NONE   "グループ: なし
        OBLIGATORY = IF_SALV_C_BOOL_SAP=>FALSE ). "必須: 無効
      LDR_ALV_SORTS->ADD_SORT(
        COLUMNNAME = 'FLDATE'                     "ソート項目: フライト日付
        POSITION   = 3                            "位置
        SEQUENCE   = IF_SALV_C_SORT=>SORT_UP      "ソート順: 昇順
        SUBTOTAL   = IF_SALV_C_BOOL_SAP=>FALSE    "小計ブレイクキー: 無効
        GROUP      = IF_SALV_C_SORT=>GROUP_NONE   "グループ: なし
        OBLIGATORY = IF_SALV_C_BOOL_SAP=>FALSE ). "必須: 無効
    CATCH CX_SALV_NOT_FOUND.
    CATCH CX_SALV_EXISTING.
    CATCH CX_SALV_DATA_ERROR.
  ENDTRY.
*
* ALVの集約を設定
  LDR_ALV_AGGREGATIONS = GDR_ALV_TABLE->GET_AGGREGATIONS( ).
  TRY.
      LDR_ALV_AGGREGATIONS->ADD_AGGREGATION(
        COLUMNNAME = 'PAYMENTSUM'                  "現在の予約合計
        AGGREGATION = IF_SALV_C_AGGREGATION=>TOTAL ).
    CATCH CX_SALV_DATA_ERROR .
    CATCH CX_SALV_NOT_FOUND .
    CATCH CX_SALV_EXISTING .
  ENDTRY.
*
* ALVのヘッダ設定
  PERFORM F_ALV_TOP_OF_PAGE CHANGING LDR_ALV_FORM_ELEMENT.
  GDR_ALV_TABLE->SET_TOP_OF_LIST( LDR_ALV_FORM_ELEMENT ).

* ALVイベント設定
  LDR_ALV_EVENTS_TABLE = GDR_ALV_TABLE->GET_EVENT( ).
  SET HANDLER lcl_alv_handle_events=>on_user_command FOR LDR_ALV_EVENTS_TABLE.
  SET HANDLER lcl_alv_handle_events=>on_double_click FOR LDR_ALV_EVENTS_TABLE.
  SET HANDLER lcl_alv_handle_events=>on_link_click FOR LDR_ALV_EVENTS_TABLE.
  SET HANDLER LCL_ALV_HANDLE_EVENTS=>ON_BEFORE_SALV_FUNCTION FOR LDR_ALV_EVENTS_TABLE.
  SET HANDLER LCL_ALV_HANDLE_EVENTS=>ON_AFTER_SALV_FUNCTION FOR LDR_ALV_EVENTS_TABLE.

* ALVを表示
  GDR_ALV_TABLE->DISPLAY( ).

ENDFORM.                    "F_ALV_WRITE

*&---------------------------------------------------------------------*
*&      Form  F_ALV_TOP_OF_PAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PRF_CONTENT  text
*----------------------------------------------------------------------*
FORM F_ALV_TOP_OF_PAGE
  CHANGING PRF_CONTENT TYPE REF TO CL_SALV_FORM_ELEMENT.

  DATA:
    LDR_ALV_FORM_LAYOUT_GRID TYPE REF TO CL_SALV_FORM_LAYOUT_GRID,
    LDR_ALV_FORM_TEXT        TYPE REF TO CL_SALV_FORM_TEXT,
    LDR_ALV_FORM_LABEL       TYPE REF TO CL_SALV_FORM_LABEL.

* 帳票レイアウトグリッドをインスタンス化
  CREATE OBJECT LDR_ALV_FORM_LAYOUT_GRID.

* ラベルを設定
  LDR_ALV_FORM_LABEL = LDR_ALV_FORM_LAYOUT_GRID->CREATE_LABEL(
    ROW     = 1
    COLUMN  = 1
    TEXT    = 'フライト日付(From)'
    TOOLTIP = 'フライト日付(From)' ).
* テキストを設定
  LDR_ALV_FORM_TEXT = LDR_ALV_FORM_LAYOUT_GRID->CREATE_TEXT(
    ROW     = 1
    COLUMN  = 2
    TEXT    = S_FLDATE-LOW
    TOOLTIP = S_FLDATE-LOW ).
* ラベルとテキストを関連付け
  LDR_ALV_FORM_LABEL->SET_LABEL_FOR( LDR_ALV_FORM_TEXT ).

* ラベルを設定
  LDR_ALV_FORM_LABEL = LDR_ALV_FORM_LAYOUT_GRID->CREATE_LABEL(
    ROW     = 1
    COLUMN  = 3
    TEXT    = 'フライト日付(To)'
    TOOLTIP = 'フライト日付(To)' ).
* テキストを設定
  LDR_ALV_FORM_TEXT = LDR_ALV_FORM_LAYOUT_GRID->CREATE_TEXT(
    ROW     = 1
    COLUMN  = 4
    TEXT    = S_FLDATE-HIGH
    TOOLTIP = S_FLDATE-HIGH ).
* ラベルとテキストを関連付け
  LDR_ALV_FORM_LABEL->SET_LABEL_FOR( LDR_ALV_FORM_TEXT ).

* ラベルを設定
  LDR_ALV_FORM_LABEL = LDR_ALV_FORM_LAYOUT_GRID->CREATE_LABEL(
    ROW     = 2
    COLUMN  = 1
    TEXT    = '該当件数'
    TOOLTIP = '該当件数' ).
* テキストを設定
  LDR_ALV_FORM_TEXT = LDR_ALV_FORM_LAYOUT_GRID->CREATE_TEXT(
    ROW     = 2
    COLUMN  = 2
    TEXT    = GDF_ALV_LINES
    TOOLTIP = GDF_ALV_LINES ).
* ラベルとテキストを関連付け
  LDR_ALV_FORM_LABEL->SET_LABEL_FOR( LDR_ALV_FORM_TEXT ).

  PRF_CONTENT = LDR_ALV_FORM_LAYOUT_GRID.

ENDFORM.                    "F_ALV_TOP_OF_PAGE

*----------------------------------------------------------------------*
*       CLASS lcl_alv_handle_events IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_ALV_HANDLE_EVENTS IMPLEMENTATION.

  METHOD ON_USER_COMMAND.

    DATA:
      LDR_ALV_SELECTIONS TYPE REF TO CL_SALV_SELECTIONS,
      LDT_SELECTED_ROWS  TYPE SALV_T_ROW,
      LDF_SELECTED_ROW   TYPE I,
      LDS_ALV_DATA       LIKE LINE OF GDT_ALV_DATA[].

    LDR_ALV_SELECTIONS = GDR_ALV_TABLE->GET_SELECTIONS( ).
    LDT_SELECTED_ROWS = LDR_ALV_SELECTIONS->GET_SELECTED_ROWS( ).

    CASE E_SALV_FUNCTION.
      WHEN 'APPROVE'.
        IF LINES( LDT_SELECTED_ROWS[] ) = 0.
          " エントリを 1 つ以上選択してください
          MESSAGE E006(0K).
        ENDIF.
        LOOP AT LDT_SELECTED_ROWS[] INTO LDF_SELECTED_ROW.
          READ TABLE GDT_ALV_DATA[] INDEX LDF_SELECTED_ROW INTO LDS_ALV_DATA.
          IF SY-SUBRC = 0.
            "XXXXXXXXXXXX
          ENDIF.
        ENDLOOP.
      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.                    "on_user_command

  METHOD ON_DOUBLE_CLICK.

    DATA:
      LDS_ALV_DATA LIKE LINE OF GDT_ALV_DATA[].

    READ TABLE GDT_ALV_DATA[] INDEX ROW INTO LDS_ALV_DATA.
    CASE COLUMN.
      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.                    "on_double_click

  METHOD ON_LINK_CLICK.

    DATA:
      LDS_ALV_DATA LIKE LINE OF GDT_ALV_DATA[].

    READ TABLE GDT_ALV_DATA[] INDEX ROW INTO LDS_ALV_DATA.
    CASE COLUMN.
      WHEN 'PLANETYPE'.
      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.                    "on_link_click


METHOD ON_BEFORE_SALV_FUNCTION.

  CASE E_SALV_FUNCTION.
    WHEN 'XXXX'.

    WHEN OTHERS.

ENDCASE.

  ENDMETHOD.                    "on_before_salv_function

  METHOD ON_AFTER_SALV_FUNCTION.

    DATA:
      LDR_ALV_SELECTIONS TYPE REF TO CL_SALV_SELECTIONS,
      LDT_SELECTED_ROWS  TYPE SALV_T_ROW,
      LDF_SELECTED_ROW   TYPE I,
      LDS_ALV_DATA       LIKE LINE OF GDT_ALV_DATA[],
      LDT_ALV_TMP        TYPE STANDARD TABLE OF SFLIGHT.

    LDR_ALV_SELECTIONS = GDR_ALV_TABLE->GET_SELECTIONS( ).
    LDT_SELECTED_ROWS = LDR_ALV_SELECTIONS->GET_SELECTED_ROWS( ).

    CASE E_SALV_FUNCTION.
      WHEN 'DOWN'.  "ローカルファイルへのエクスポート
*        IF GCF_ALV_ENABLE_AUDIT_LOG = 'X'.
        IF LINES( LDT_SELECTED_ROWS[] ) = 0.
          " エントリを 1 つ以上選択してください
          MESSAGE E006(0K).
        ELSE.

          LOOP AT LDT_SELECTED_ROWS[] INTO LDF_SELECTED_ROW.
            READ TABLE GDT_ALV_DATA[] INDEX LDF_SELECTED_ROW INTO LDS_ALV_DATA.
            IF SY-SUBRC = 0.
              APPEND LDS_ALV_DATA to LDT_ALV_TMP.
            ENDIF.
          ENDLOOP.
          PERFORM F_DOWNLOAD_AUDIT_LOG USING LDT_ALV_TMP[].
        ENDIF.
      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.                    "on_after_salv_function
ENDCLASS.                    "lcl_alv_handle_events IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Form  F_DOWNLOAD_AUDIT_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->PRT_ALV_DATA  text
*----------------------------------------------------------------------*
FORM F_DOWNLOAD_AUDIT_LOG
  USING PRT_ALV_DATA TYPE STANDARD TABLE.

  DATA:
    LDF_FILENAME    TYPE STRING,
    LDF_HEADER_LINE TYPE STRING.

  CONCATENATE SY-CPROG SY-UNAME SY-DATUM SY-UZEIT INTO LDF_HEADER_LINE SEPARATED BY SPACE.
  CONCATENATE 'D:\EVANTEST\' 'ZDLLOG_' SY-UNAME SY-DATUM SY-UZEIT INTO LDF_FILENAME.

  CALL FUNCTION 'ZTL_BCF010L_DOWNLOAD'
    EXPORTING
      FILENAME            = LDF_FILENAME
      FILETYPE            = 'ASC'
      LOCAL               = SPACE
      SEPARATOR           = CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB
      HEADER_LINE         = LDF_HEADER_LINE
    TABLES
      USERTABLE           = PRT_ALV_DATA[]
    EXCEPTIONS
      FILE_OPEN_ERROR     = 1
      DIRECTORY_NOT_FOUND = 2
      OTHERS              = 3.
  IF SY-SUBRC <> 0.
    ASSERT 0 = 1.
  ENDIF.

ENDFORM.                    "F_DOWNLOAD_AUDIT_LOG
