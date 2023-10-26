CLASS sel_screen_handler IMPLEMENTATION.

  METHOD constructor.
    "-----------------------------------------------------------------*
    "   Construtcor
    "-----------------------------------------------------------------*
    parent = io_flight_search.

    flight_model = NEW #( ).

    scr_fld_attr = zcl_ca_c_screen_field_attr=>get_instance( ).

    get_available_flightdate_range( ).
    set_default_values( ).
    get_airports_for_value_req( ).
  ENDMETHOD.                    "constructor


  METHOD get_available_flightdate_range.
    "-----------------------------------------------------------------*
    "   Prepare airport names for listbox
    "-----------------------------------------------------------------*
    SELECT MIN( fldate ) AS lowest                      "#EC CI_NOWHERE
           MAX( fldate ) AS hightest
                         INTO flight_date_range
                         FROM sflights2.

    "Currently available flight dates are between dd.mm.yyyy and dd.mm.yyyy
    txcm_tdd = |{ TEXT-tdd } { flight_date_range-lowest  DATE = USER } | &
               |{ TEXT-and } { flight_date_range-highest DATE = USER }|.
  ENDMETHOD.                    "get_available_flightdate_range


  METHOD get_airports_for_value_req.
    "-----------------------------------------------------------------*
    "   Prepare airport names for listbox
    "-----------------------------------------------------------------*
    IF airports_from IS INITIAL.
      SELECT id   AS key,                               "#EC CI_NOWHERE
             name AS text FROM sairport
                          INTO TABLE @airports_from.
    ENDIF.

    SELECT DISTINCT schedule~airpto,
           airport~id   AS key,
           airport~name AS text
                        INTO  CORRESPONDING FIELDS OF TABLE @airports_to
                        FROM  spfli AS schedule        "#EC CI_BUFFJOIN
                              INNER JOIN sairport AS airport
                                      ON airport~id EQ schedule~airpto
                        WHERE schedule~airpfrom EQ @p_airpfr.
  ENDMETHOD.                    "get_airports_for_value_req


  METHOD set_default_values.
    "-----------------------------------------------------------------*
    "   Prepare airport names for listbox
    "-----------------------------------------------------------------*
    "After initial data creation no connection from Vienna available
    p_airpfr = 'FRA' ##no_text.
    SET CURSOR FIELD 'P_AIRPTO' ##no_text.
    p_flddep = flight_date_range-lowest + 3.    "Departure date
    p_fldret = p_flddep + 3.                    "Returning date
  ENDMETHOD.                    "set_default_values


  METHOD at_sel_screen_output.
    "-----------------------------------------------------------------*
    "   Control / adjust selection screen fields
    "-----------------------------------------------------------------*
    flight_model->at_sel_screen_output( ).

    set_airports_for_listbox( ).
    adjust_selection_screen( ).
  ENDMETHOD.                    "at_sel_screen_output


  METHOD set_airports_for_listbox.
    "-----------------------------------------------------------------*
    "   Set airport values for listbox
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lx_error             TYPE REF TO zcx_ca_intern.

    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id              = 'P_AIRPFR'
        values          = airports_from
      EXCEPTIONS
        id_illegal_name = 1
        OTHERS          = 2 ##no_text.

    lx_error = zcx_ca_intern=>create_exception(
                         iv_excp_cls = zcx_ca_intern=>c_zcx_ca_intern
                         iv_function = 'VRM_SET_VALUES / P_AIRPFR'
                         iv_subrc    = sy-subrc ) ##no_text.
    IF lx_error IS BOUND.
      RAISE EXCEPTION lx_error.
    ENDIF.

    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id              = 'P_AIRPTO'
        values          = airports_to
      EXCEPTIONS
        id_illegal_name = 1
        OTHERS          = 2 ##no_text.

    lx_error = zcx_ca_intern=>create_exception(
                         iv_excp_cls = zcx_ca_intern=>c_zcx_ca_intern
                         iv_function = 'VRM_SET_VALUES / P_AIRPTO'
                         iv_subrc    = sy-subrc ) ##no_text.
    IF lx_error IS BOUND.
      RAISE EXCEPTION lx_error.
    ENDIF.
  ENDMETHOD.                    "set_airports_for_listbox


  METHOD adjust_selection_screen.
    "-----------------------------------------------------------------*
    "   Adjust selection screen to current settings
    "-----------------------------------------------------------------*
    LOOP AT SCREEN INTO DATA(field).
      CASE field-name.
        WHEN 'P_AIRPFR' ##no_text.
          field-required  = scr_fld_attr->switch-is_recommended.
        WHEN 'P_AIRPTO' ##no_text.
          field-required  = scr_fld_attr->switch-is_recommended.
        WHEN 'P_FLDDEP' ##no_text.
          field-required  = scr_fld_attr->switch-is_recommended.
        WHEN 'P_FLDRET' ##no_text.
          field-required  = scr_fld_attr->switch-is_recommended.
      ENDCASE.

      CASE field-group1.
        WHEN 'RET' ##no_text.
          field-invisible = SWITCH #( p_tripty WHEN parent->trip_type-round  THEN scr_fld_attr->switch-off
                                               WHEN parent->trip_type-single THEN scr_fld_attr->switch-on ).
          field-active    = SWITCH #( p_tripty WHEN parent->trip_type-round  THEN scr_fld_attr->switch-on
                                               WHEN parent->trip_type-single THEN scr_fld_attr->switch-off ).
      ENDCASE.

      MODIFY SCREEN FROM field.
    ENDLOOP.
  ENDMETHOD.                    "adjust_selection_screen


  METHOD at_sel_screen.
    "-----------------------------------------------------------------*
    "   Check selection values and handle user commands
    "-----------------------------------------------------------------*
    TRY.
        CASE sscrfields-ucomm.
          WHEN 'ONLI' ##no_text.
            flight_model->create_data( ).
            check_selection_input( ).

          WHEN 'AIRPORT_FROM_SEL' ##no_text.
            get_airports_for_value_req( ).

          WHEN OTHERS.
            flight_model->at_sel_screen( ).
        ENDCASE.

      CATCH zcx_ca_error INTO DATA(lx_catched).
        MESSAGE lx_catched TYPE lx_catched->mv_msgty.
    ENDTRY.
  ENDMETHOD.                    "at_sel_screen


  METHOD check_selection_input.
    "-----------------------------------------------------------------*
    "   Check entered selections
    "-----------------------------------------------------------------*
    "???
  ENDMETHOD.                    "check_selection_input


  METHOD at_sel_screen_block_conns.
    "-----------------------------------------------------------------*
    "   Check selection values on block 'Connections'
    "-----------------------------------------------------------------*
    TRY.
        IF p_airpfr IS INITIAL.
          RAISE EXCEPTION TYPE zcx_ca_param
            MESSAGE ID '38' TYPE msg_type_e NUMBER 001 WITH
            'Obligatory value for'(se1)
            'Departing airport'(dpa)
            'is not set'(se2)
            space.
        ENDIF.

        IF p_airpto IS INITIAL.
          RAISE EXCEPTION TYPE zcx_ca_param
            MESSAGE ID '38' TYPE msg_type_e NUMBER 001 WITH
            'Obligatory value for'(se1)
            'Arrival airport'(dea)
            'is not set'(se2)
            space.
        ENDIF.

      CATCH zcx_ca_error INTO DATA(lx_catched).
        MESSAGE lx_catched TYPE lx_catched->mv_msgty.
    ENDTRY.
  ENDMETHOD.                    "at_sel_screen_block_conns


  METHOD at_sel_screen_block_dates.
    "-----------------------------------------------------------------*
    "   Check selection values on block 'Travel dates'
    "-----------------------------------------------------------------*
    TRY.
        IF p_flddep NOT BETWEEN flight_date_range-lowest  AND
                                flight_date_range-highest.
          RAISE EXCEPTION TYPE zcx_ca_param
            MESSAGE ID '38' TYPE msg_type_e NUMBER 001 WITH
            'Obligatory departure date is out of the'(se3)
            'available schedule range'(se4)
            space
            space.
        ENDIF.

        IF p_flddep NOT BETWEEN flight_date_range-lowest  AND
                                flight_date_range-highest.
          RAISE EXCEPTION TYPE zcx_ca_param
            MESSAGE ID '38' TYPE msg_type_e NUMBER 001 WITH
            'Obligatory return flight date is out of the'(se5)
            'available schedule range'(se4)
            space
            space.
        ENDIF.

      CATCH zcx_ca_error INTO DATA(lx_catched).
        MESSAGE lx_catched TYPE lx_catched->mv_msgty.
    ENDTRY.
  ENDMETHOD.                    "at_sel_screen_block_dates

ENDCLASS.                     "sel_screen_handler  IMPLEMENTATION
