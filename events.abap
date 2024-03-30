*&---------------------------------------------------------------------*
*& Report ZCREATE_TMG
*&---------------------------------------------------------------------*
"There is no need of using an event and triggering this code,
"you can choose below ways to do the same thing.
"however, this code should make you aware of how event
"handler methods are executed in real-time.
"in custom event handler methods, you have to create and raise the
"event by yourself, and handle it later on.
"while in case of standard events, SAP takes care of creating
"and raising the events, you have to handle the events.
REPORT zcreate_tmg.
CLASS lcl_tmg DEFINITION.
  PUBLIC SECTION.
    EVENTS : e1.
    CLASS-METHODS : save.
    METHODS : constructor,
      get_data,
      get_alv,
      display_alv,
      field_cat,
      update FOR EVENT e1 OF lcl_tmg,
      modify.
  PRIVATE SECTION.
    CLASS-DATA : itab TYPE TABLE OF zsales.
    CLASS-DATA : o_alv TYPE REF TO cl_gui_alv_grid.
    DATA : fcat TYPE lvc_t_fcat.
    DATA : cl_tmg TYPE REF TO lcl_tmg.
ENDCLASS.
CLASS lcl_tmg IMPLEMENTATION.
  METHOD constructor.
    get_alv( ).
  ENDMETHOD.
  METHOD modify.
    RAISE EVENT e1.
  ENDMETHOD.
  METHOD update.
    IF NOT o_alv IS INITIAL.
      CALL METHOD o_alv->check_changed_data.
    ENDIF.
    MODIFY zsales FROM TABLE itab.
    COMMIT WORK.
  ENDMETHOD.
  METHOD get_alv.
    CREATE OBJECT o_alv EXPORTING i_parent = cl_gui_container=>default_screen.
  ENDMETHOD.
  METHOD display_alv.
    DATA : ls_layout TYPE lvc_s_layo.
    get_data( ).
    field_cat( ).
    ls_layout-grid_title = 'MAINTAIN DATA'.
    o_alv->set_table_for_first_display(
    EXPORTING
    i_default = 'X' " Default Display Variant
    CHANGING
    it_outtab = itab " Output Table
    it_fieldcatalog = fcat " Field Catalog
    ).
    CALL SCREEN 100.
* cl_abap_list_layout=>suppress_toolbar( ).
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
* WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
  ENDMETHOD.
  METHOD field_cat.
    cl_salv_table=>factory(
    IMPORTING
    r_salv_table = DATA(lo_table) " Basis Class Simple ALV Tables
    CHANGING
    t_table = itab
    ).
    DATA(fieldcat) = cl_salv_controller_metadata=>get_lvc_fieldcatalog(
    EXPORTING
    r_columns = lo_table->get_columns( )
    r_aggregations = lo_table->get_aggregations( )  ).
    fcat = VALUE #( FOR wcat IN fieldcat (  edit = 'X'
                                            fieldname = wcat-fieldname
                                            scrtext_s = wcat-scrtext_s
                                            scrtext_m = wcat-scrtext_m
                                            scrtext_l = wcat-scrtext_l
                                           ) ).
  ENDMETHOD.
  METHOD get_data.
    SELECT * FROM zsales INTO TABLE @itab.
  ENDMETHOD.
  METHOD save.
    IF NOT o_alv IS INITIAL.
      CALL METHOD o_alv->check_changed_data.
    ENDIF.
    MODIFY zsales FROM TABLE itab.
    COMMIT WORK.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA(cl_tmg) = NEW lcl_tmg( ).
  cl_tmg->display_alv( ).
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'ZPF_STAT'.
* SET TITLEBAR 'xxx'.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module USER_COMMAND_0100 INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN 'SAVE'.
      SET HANDLER cl_tmg->update FOR cl_tmg.
      cl_tmg->modify( ).
* lcl_tmg=>save( ). "You can do this way,
* cl_tmg->save( ).
  ENDCASE.
ENDMODULE.
