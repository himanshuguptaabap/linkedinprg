"These are very different programs which you"can hardly get your hands-on on google but these progr"will come handy when you have large amount of data
"or you have to do things fast.
"I created this program for myself and others
"who are a bit lazy but they want to do the work in"a smart way.
REPORT zdynamic_programing.
CLASS lcl_excel DEFINITION.
  PUBLIC SECTION.
    TYPES : tt_fields TYPE TABLE OF dd04t,
            tt_comp   TYPE cl_abap_structdescr=>component_table,
            ty_bin    TYPE solix_tab..
    METHODS : constructor,
              get_string_data     EXPORTING ex_STRING TYPE string.
    
  PROTECTED SECTION.
    METHODS : get_parameters      EXPORTING ex_lines  TYPE int4
                                            ex_fields TYPE tt_fields
                                            ex_comp   TYPE tt_comp,
             create_columns       IMPORTING im_lines TYPE int4,
             create_dynamic_table CHANGING  ex_ref TYPE REF TO data,
             set_header           IMPORTING im_fields TYPE tt_fields
                                            im_comp   TYPE tt_comp
                                  CHANGING  ex_ref    TYPE REF TO data,
             set_data             IMPORTING im_lines TYPE int4
                                            im_comp  TYPE tt_comp
                                  CHANGING  ex_ref   TYPE REF TO data,
             create_string        IMPORTING im_ref TYPE REF TO data.
  
  PRIVATE SECTION.
    DATA : it_makt TYPE TABLE OF makt.
    DATA : bin TYPE solix_tab.
    DATA : it_comp TYPE cl_abap_structdescr=>component_table.
    DATA : lr_ref TYPE REF TO data.
    DATA : ls_string TYPE string.
ENDCLASS.

CLASS lcl_excel IMPLEMENTATION.
  METHOD constructor.
    "These are very different programs which you
    "can hardly get your hands-on on google but these programs
    "will come handy when you have large amount of data
    "or you have to do things fast.
    "I created this program for myself and others
    "who are a bit lazy but they want to do the work in
    "a smart way.
    SELECT * FROM makt INTO TABLE @me->it_makt UP TO 10 ROWS.
    me->get_parameters(
            IMPORTING
              ex_lines = DATA(lv_lines)
              ex_fields = DATA(lt_fields)
              ex_comp = DATA(lt_comp)
                      ).
    me->create_columns(
           EXPORTING
              im_lines = lv_lines
                       ).
    me->create_dynamic_table(
           CHANGING
              ex_REF = lr_ref
                            ).
    me->set_header(
          EXPORTING
            im_comp = lt_comp
            im_fields = lt_fields
          CHANGING
            ex_ref = lr_ref
                  ).
    me->set_data(
          EXPORTING
            im_lines = lv_lines
            im_comp = lt_comp
          CHANGING
            ex_ref = lr_ref
                ).
    me->create_string(
          EXPORTING
           im_ref = lr_ref
                     ).
  ENDMETHOD.
  METHOD get_parameters.
    DATA(lo_table) = CAST cl_abap_tabledescr(
    cl_abap_typedescr=>describe_by_data(
    EXPORTING p_data = it_makt )
    ).
    DATA(lt_components) = CAST cl_abap_structdescr(
    lo_table->get_table_line_type( )
    )->get_components( ).
    DATA(lv_lines) = lines( lt_components ).
    DATA(lr_names) = VALUE rsdsselopt_t( FOR wa2
    IN lt_components
   ( sign = 'I' option = 'EQ' low = wa2-name )
    ).
    SELECT * FROM dd04t
    INTO TABLE @DATA(lt_fields)
    WHERE rollname IN @lr_names
    AND ddlanguage EQ 'E'.
    ex_lines = lv_lines.
    ex_fields = lt_fields.
    ex_comp = lt_components.
  ENDMETHOD.
  METHOD create_columns.
    DATA : num TYPE int4.
    DATA : column_name TYPE char50.
    DATA: lt_comp TYPE cl_abap_structdescr=>component_table.
    DO im_lines TIMES.
      num = num + 1.
      column_name = 'COLUMN'.
      APPEND INITIAL LINE TO lt_comp ASSIGNING FIELD-SYMBOL(<fs_comp2>).
      <fs_comp2>-name = column_name && num.
      <fs_comp2>-type = cl_abap_elemdescr=>get_c( 40 ).
    ENDDO.
    it_comp = lt_comp.
  ENDMETHOD.
  METHOD create_dynamic_table.
    DATA : itab_new TYPE REF TO data.
    FIELD-SYMBOLS : <fs_tab_new> TYPE INDEX TABLE.
    DATA(lo_struct) = cl_abap_structdescr=>create( p_components = it_comp ).
    DATA(lo_table) = cl_abap_tabledescr=>create(
    p_line_type = lo_struct
   p_table_kind = cl_abap_tabledescr=>tablekind_std
    ).
    CREATE DATA itab_new TYPE HANDLE lo_table.
    ASSIGN itab_new->* TO <fs_tab_new>.
    ex_ref = itab_new.
  ENDMETHOD.
  METHOD create_string.
    DATA : lv_string TYPE string.
    DATA : lv_string2 TYPE string.
    DATA : lv_bin TYPE solix_tab.
    DATA : lv_xstring TYPE xstring.
    FIELD-SYMBOLS : <fs_tab_new> TYPE INDEX TABLE.
    ASSIGN im_ref->* TO <fs_tab_new>.
    LOOP AT <fs_tab_new>
    ASSIGNING FIELD-SYMBOL(<fs_s4>).
      LOOP AT it_comp INTO DATA(wa_data2).
        ASSIGN COMPONENT wa_data2-name OF STRUCTURE <fs_s4>
        TO FIELD-SYMBOL(<fs_s5>).
        IF sy-subrc IS INITIAL.
          CONCATENATE lv_string <fs_s5> INTO lv_string
          SEPARATED BY cl_abap_char_utilities=>horizontal_tab.
        ENDIF.
      ENDLOOP.
      CONCATENATE lv_string2 lv_string INTO lv_string2
      SEPARATED BY cl_abap_char_utilities=>newline.
      CLEAR : lv_string.
    ENDLOOP.
    ls_string = lv_string2.
  ENDMETHOD.
  METHOD set_data.
    FIELD-SYMBOLS : <fs_tab_new> TYPE INDEX TABLE.
    ASSIGN ex_ref->* TO <fs_tab_new>.
    LOOP AT it_makt INTO DATA(wa).
      APPEND INITIAL LINE TO <fs_tab_new>
      ASSIGNING FIELD-SYMBOL(<fs_s>).
      LOOP AT im_comp INTO DATA(wa_comp).
        DO im_lines TIMES.
          ASSIGN COMPONENT sy-index OF STRUCTURE wa
          TO FIELD-SYMBOL(<fs_s2>).
          IF sy-subrc IS INITIAL.
            ASSIGN COMPONENT sy-index OF STRUCTURE <fs_s>
            TO FIELD-SYMBOL(<fs_s3>).
            IF sy-subrc IS INITIAL.
              <fs_s3> = <fs_s2>.
            ENDIF.
          ENDIF.
        ENDDO.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.
  METHOD set_header.
    DATA : column TYPE int4.
    FIELD-SYMBOLS : <fs_tab_new> TYPE INDEX TABLE.
    ASSIGN ex_ref->* TO <fs_tab_new>.
    APPEND INITIAL LINE TO <fs_tab_new>
    ASSIGNING FIELD-SYMBOL(<fs_s7>).
    LOOP AT im_comp INTO DATA(wa_comp).
      column = column + 1.
      ASSIGN COMPONENT column OF STRUCTURE <fs_s7>
      TO FIELD-SYMBOL(<fs_s8>).
      IF sy-subrc IS INITIAL.
        <fs_s8> = VALUE #( im_fields[ rollname = wa_comp-name ]-scrtext_m OPTIONAL ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD get_STRING_data.
    ex_STRING = ls_string.
  ENDMETHOD.
ENDCLASS.

CLASS cl_send_mail DEFINITION.
  PUBLIC SECTION.
    METHODS : constructor
      IMPORTING im_str TYPE string.
  PROTECTED SECTION.
    METHODS : create_msg ,
      create_document,
      convert_to_bin
        IMPORTING im_string  TYPE string
        EXPORTING ex_content TYPE solix_tab
                  ex_size    TYPE so_obj_len,
      add_attachment
        IMPORTING im_content TYPE solix_tab
                  im_size    TYPE so_obj_len,
      set_document,
      add_receipent,
      add_sender,
      send_req.
  PRIVATE SECTION.
    DATA : lo_send_email TYPE REF TO cl_bcs.
    DATA : lo_document TYPE REF TO cl_document_bcs.
ENDCLASS.

CLASS cl_send_mail IMPLEMENTATION.
  METHOD : constructor.
    me->convert_to_bin(
         EXPORTING
            im_string = im_str
         IMPORTING
            ex_content = DATA(lt_content)
            ex_size = DATA(lt_size) ).
    me->create_msg( ) .
    me->create_document( ).
    me->add_attachment(
         EXPORTING
            im_content = lt_content
            im_size = lt_size ).
    me->set_document( ).
    me->add_sender( ).
    me->add_receipent( ).
    me->send_req( ).
  ENDMETHOD.
  METHOD set_document.
    lo_send_email->set_document( lo_document ).
  ENDMETHOD.
  METHOD : create_msg.
    lo_send_email = cl_bcs=>create_persistent( ).
  ENDMETHOD.
  METHOD create_document.
    lo_document = cl_document_bcs=>create_document(
    i_type = 'RAW'
    i_text = VALUE #( ( line = 'Dear Recipient,' )
                      ( line = 'This is Test Email Program.' )
                      ( line = 'Thank You' ) )
                      i_subject = 'Test Email' ).
  ENDMETHOD.
  METHOD add_attachment.
    lo_document->add_attachment(
    i_attachment_type = 'xls'
    i_attachment_subject = 'Excel File'
    i_attachment_size = im_size
    i_att_content_hex = im_content ).
  ENDMETHOD.
  METHOD convert_to_bin.
    cl_bcs_convert=>string_to_solix(
    EXPORTING
    iv_string = im_string
    iv_codepage = '4103' "suitable for MS Excel, leave empty
    iv_add_bom = 'X' "for other doc types
    IMPORTING
    et_solix = ex_content
    ev_size = ex_size ).
  ENDMETHOD.
  METHOD : add_receipent.
    lo_send_email->add_recipient( cl_cam_address_bcs=>create_internet_address( CONV #( 'receiver@dummy.com' ) ) ).
  ENDMETHOD.
  METHOD : add_sender.
    lo_send_email->set_sender( cl_cam_address_bcs=>create_internet_address( i_address_string = CONV #( 'sender@dummy.com' ) ) ).
  ENDMETHOD.
  METHOD : send_req.
    DATA(lv_flag) = lo_send_email->send( ).
    MESSAGE | { SWITCH #( lv_flag WHEN 'X' THEN 'Sent'
    WHEN '' THEN 'Error') } | TYPE 'I'.
    COMMIT WORK.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA(lr_obj) = NEW lcl_excel( ).
  lr_obj->get_STRING_data(
                         IMPORTING
                         ex_STRING = DATA(ls_string)
                         ).
  DATA(lr_email) = NEW cl_send_mail( im_str = ls_string ).
