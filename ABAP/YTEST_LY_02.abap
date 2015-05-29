*&---------------------------------------------------------------------*
*& Report  YTEST_LY_02
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT  YTEST_LY_02.
INCLUDE <ICON>.
*变量定义
TYPES: BEGIN OF GS_SPFLI,
        MANDT       TYPE SPFLI-MANDT,
        CARRID      TYPE SPFLI-CARRID,
        CONNID      TYPE SPFLI-CONNID,
        COUNTRYFR   TYPE SPFLI-COUNTRYFR,
        CITYFROM    TYPE SPFLI-CITYFROM,
        AIRPFROM    TYPE SPFLI-AIRPFROM,
        COUNTRYTO   TYPE SPFLI-COUNTRYTO,
        CITYTO      TYPE SPFLI-CITYTO,
        AIRPTO      TYPE SPFLI-AIRPTO,
        FLTIME      TYPE SPFLI-FLTIME,
        DEPTIME     TYPE SPFLI-DEPTIME,
        ARRTIME     TYPE SPFLI-ARRTIME,
        DISTANCE    TYPE SPFLI-DISTANCE,
        DISTID      TYPE SPFLI-DISTID,
        FLTYPE      TYPE SPFLI-FLTYPE,
        PERIOD      TYPE SPFLI-PERIOD,
        HOTSPOT     TYPE ICON_D,
END OF GS_SPFLI.
TYPES  TY_SPFLI TYPE GS_SPFLI OCCURS 0.
*----------------------------------------------------------------------*
*       CLASS lcl_handle_events DEFINITION
*----------------------------------------------------------------------*
*       事件操作类（定义）
*----------------------------------------------------------------------*
CLASS LCL_HANDLE_EVENTS DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS: ON_USER_COMMAND FOR EVENT ADDED_FUNCTION OF CL_SALV_EVENTS_TABLE
                    IMPORTING E_SALV_FUNCTION,
                   ON_DOUBLE_CLICK FOR EVENT DOUBLE_CLICK OF CL_SALV_EVENTS_TABLE
                    IMPORTING ROW COLUMN,
                   ON_LINK_CLICK   FOR EVENT LINK_CLICK OF CL_SALV_EVENTS_TABLE
                    IMPORTING ROW COLUMN,
                   ON_BEFORE_SALV_FUNCTION FOR EVENT BEFORE_SALV_FUNCTION OF CL_SALV_EVENTS_TABLE
                    IMPORTING E_SALV_FUNCTION,
                   ON_AFTER_SALV_FUNCTION FOR EVENT AFTER_SALV_FUNCTION OF CL_SALV_EVENTS_TABLE
                    IMPORTING E_SALV_FUNCTION.
  PRIVATE SECTION.
    CLASS-DATA: G_STRING TYPE STRING.
ENDCLASS.                    "lcl_handle_events DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_handle_events IMPLEMENTATION
*----------------------------------------------------------------------*
*       事件操作类（实现）
*----------------------------------------------------------------------*
CLASS LCL_HANDLE_EVENTS IMPLEMENTATION.
*单击新增功能按钮事件处理方法
  METHOD ON_USER_COMMAND.
    CONCATENATE 'Function Code is '
                E_SALV_FUNCTION
           INTO G_STRING SEPARATED BY SPACE.
    MESSAGE I000(0K) WITH G_STRING.
  ENDMETHOD.                    "on_user_command
*双击事件处理方法
  METHOD ON_DOUBLE_CLICK.
    G_STRING = ROW.
    CONCATENATE 'DOUBLE CLICK'
                'ROW'
                G_STRING
                'COLUMN'
                COLUMN
           INTO G_STRING SEPARATED BY SPACE.
    MESSAGE I000(0K) WITH G_STRING.
  ENDMETHOD.                    "on_double_click
*单击事件处理方法
  METHOD ON_LINK_CLICK.
    G_STRING = ROW.
    CONCATENATE 'LICK CLICK'
                'ROW'
                G_STRING
                'COLUMN'
                COLUMN
           INTO G_STRING SEPARATED BY SPACE.
    MESSAGE I000(0K) WITH G_STRING.
  ENDMETHOD.                    "on_link_click
*触发事件之前
  METHOD ON_BEFORE_SALV_FUNCTION.
    CONCATENATE 'Before Function '
                E_SALV_FUNCTION
           INTO G_STRING SEPARATED BY SPACE.
    MESSAGE I000(0K) WITH G_STRING.
  ENDMETHOD.                    "on_before_salv_function
*触发事件之后
  METHOD ON_AFTER_SALV_FUNCTION.
    CONCATENATE 'After Function '
                E_SALV_FUNCTION
           INTO G_STRING SEPARATED BY SPACE.
    MESSAGE I000(0K) WITH G_STRING.
  ENDMETHOD.                    "on_after_salv_function
ENDCLASS.                    "lcl_handle_events IMPLEMENTATION
*----------------------------------------------------------------------*
*       CLASS lcl_alv DEFINITION
*----------------------------------------------------------------------*
*       ALV操作类（定义）
*----------------------------------------------------------------------*
CLASS LCL_ALV DEFINITION.
  PUBLIC SECTION.
    METHODS: GETDATA               "取得要显示的数据
               RETURNING VALUE(LT_TAB) TYPE TY_SPFLI,
             ALV_FULL              "全屏Grid列表处理方法
               IMPORTING VALUE(LT_TAB) TYPE TY_SPFLI.
  PRIVATE SECTION.
  DATA: GR_TABLE TYPE REF TO CL_SALV_TABLE.
ENDCLASS.                    "lcl_alv DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_alv IMPLEMENTATION
*----------------------------------------------------------------------*
*       ALV操作类（实现）
*----------------------------------------------------------------------*
CLASS LCL_ALV IMPLEMENTATION.
*取得要显示的数据
  METHOD GETDATA.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE LT_TAB FROM SPFLI.
    FIELD-SYMBOLS <FS_SPFLI> TYPE GS_SPFLI.
    LOOP AT LT_TAB ASSIGNING <FS_SPFLI>.
      <FS_SPFLI>-HOTSPOT = ICON_OVERVIEW.
    ENDLOOP.
  ENDMETHOD.                    "getdata
*输出全屏网格列表的方法
  METHOD ALV_FULL.
    DATA: LR_FUNCTIONS  TYPE REF TO CL_SALV_FUNCTIONS_LIST,
          LR_EVENTS     TYPE REF TO CL_SALV_EVENTS_TABLE,
          LR_COLUMNS    TYPE REF TO CL_SALV_COLUMNS_TABLE,
          LR_COLUMN     TYPE REF TO CL_SALV_COLUMN_TABLE,
          LR_CONTENT    TYPE REF TO CL_SALV_FORM_ELEMENT.
    "创建实例
    TRY.
        CL_SALV_TABLE=>FACTORY(
          IMPORTING
            R_SALV_TABLE = GR_TABLE
          CHANGING
            T_TABLE      = LT_TAB
        ).
      CATCH CX_SALV_MSG.
    ENDTRY.
    "设置GUI Status
    GR_TABLE->SET_SCREEN_STATUS(
      REPORT = SY-REPID
      PFSTATUS = 'ZSALV_STANDARD'
      SET_FUNCTIONS = GR_TABLE->C_FUNCTIONS_ALL
    ).
    "设置热点
    LR_COLUMNS = GR_TABLE->GET_COLUMNS( ).
    TRY.
        LR_COLUMN ?= LR_COLUMNS->GET_COLUMN( 'HOTSPOT' ).
        LR_COLUMN->SET_CELL_TYPE( IF_SALV_C_CELL_TYPE=>HOTSPOT ).
        LR_COLUMN->SET_LONG_TEXT( 'HOTSPOT' ).
      CATCH CX_SALV_NOT_FOUND.
    ENDTRY.
    "注册事件
    LR_EVENTS = GR_TABLE->GET_EVENT( ).
    SET HANDLER LCL_HANDLE_EVENTS=>ON_USER_COMMAND FOR LR_EVENTS.
    SET HANDLER LCL_HANDLE_EVENTS=>ON_DOUBLE_CLICK FOR LR_EVENTS.
    SET HANDLER LCL_HANDLE_EVENTS=>ON_LINK_CLICK FOR LR_EVENTS.
    SET HANDLER LCL_HANDLE_EVENTS=>ON_BEFORE_SALV_FUNCTION FOR LR_EVENTS.
    SET HANDLER LCL_HANDLE_EVENTS=>ON_AFTER_SALV_FUNCTION FOR LR_EVENTS.
    "显示列表
    GR_TABLE->DISPLAY( ).
  ENDMETHOD.                    "alv_full
ENDCLASS.                    "lcl_alv IMPLEMENTATION
*&---------------------------------------------------------------------*
*&      Form  f_main
*&---------------------------------------------------------------------*
*       整合数据，执行
*----------------------------------------------------------------------*
FORM F_MAIN.
  DATA: LT_TAB TYPE TY_SPFLI,
        LR_ALV TYPE REF TO LCL_ALV.
  CREATE OBJECT LR_ALV.
  "取得要显示的数据
  LT_TAB = LR_ALV->GETDATA( ).
  LR_ALV->ALV_FULL( LT_TAB ).
ENDFORM.                    "f_main

*执行动作
START-OF-SELECTION.
  PERFORM F_MAIN.
