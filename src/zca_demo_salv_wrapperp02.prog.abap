CLASS search_flight_connections IMPLEMENTATION.

  METHOD constructor.
    "-----------------------------------------------------------------*
    "   Initialization of report data / selections
    "-----------------------------------------------------------------*
    super->constructor(
      EXPORTING
        ir_table              = REF #( flights )
*        iv_prg_title          = SY-TITLE
*        iv_list_title         =
*        iv_register_events    = abap_false
*        iv_layout_restriction = c_layo_restr_none
*        iv_prepare_default    = abap_true
*        iv_prg_variants       = SY-CPROG
*        iv_layout_handle      =
*        io_container          =
*        iv_cnt_name           =
    ).

    sel_screen = NEW #( me ).
  ENDMETHOD.                    "constructor


  METHOD process.
    "-----------------------------------------------------------------*
    "   Main method, that controls the entire processing
    "-----------------------------------------------------------------*
    TRY.
        get_data( ).

        prepare_alv( ).
        mo_salv->display( ).

      CATCH cx_salv_error INTO DATA(lx_catched_salv).
        MESSAGE lx_catched_salv TYPE c_msgty_e.

      CATCH zcx_ca_error INTO DATA(lx_catched).
        MESSAGE lx_catched TYPE lx_catched->c_msgty_s DISPLAY LIKE lx_catched->mv_msgty.
    ENDTRY.
  ENDMETHOD.                    "process


  METHOD prepare_alv.
    "-----------------------------------------------------------------*
    "   Adjust SALV - columns are already available
    "-----------------------------------------------------------------*
    LOOP AT mt_cols REFERENCE INTO DATA(column).
      DATA(table_column) = CAST cl_salv_column_table( column->r_column ).  "Has more options to manipulate

      CASE column->columnname.
        WHEN 'COUNTRYFR' ##no_text.
          table_column->set_short_text( CONV #( 'Dep.Ctry.'(dcs) ) ).
          table_column->set_medium_text( CONV #( 'Dep. Country'(dcm) ) ).
          table_column->set_long_text( CONV #( 'Departing Country'(dcl) ) ).

        WHEN 'COUNTRYTO' ##no_text.
          table_column->set_short_text( CONV #( 'Arr.Ctry.'(acs) ) ).
          table_column->set_medium_text( CONV #( 'Arr. Country'(acm) ) ).
          table_column->set_long_text( CONV #( 'Arriving Country'(acl) ) ).

        WHEN 'DEPTIME' OR 'ARRTIME' ##no_text.
          table_column->set_edit_mask( '__:__' ).   "Time w/o seconds

        WHEN 'SEATSMAX'   OR 'SEATSOCC'   OR 'SEATSMAX_B' OR 'SEATSOCC_B' OR
             'SEATSMAX_F' OR 'SEATSOCC_F' OR 'PLANETYPE'  OR 'FLTIME' ##no_text.
          table_column->set_visible( abap_false ).     "Hide -> can be displayed by changing layout

        WHEN 'DISTANCE'  OR 'DISTID'    OR 'FLTYPE'    OR 'PERIOD'    OR
             'PAYMENTSUM' OR 'MANDT' ##no_text.
          table_column->set_technical( ).  "Not available
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.                    "prepare_alv


  METHOD get_data.
    "-----------------------------------------------------------------*
    "   Main method, that controls the entire processing
    "-----------------------------------------------------------------*
    CASE p_tripty.
      WHEN trip_type-single.
        get_flights_for_single_trip( ).

      WHEN trip_type-round.
        get_flights_for_round_trip( ).

      WHEN trip_type-multiple.

    ENDCASE.

    IF flights IS INITIAL.
      "No data was found for the specified selection criteria
      RAISE EXCEPTION TYPE zcx_ca_dbacc
        EXPORTING
          textid = zcx_ca_dbacc=>no_data.
    ENDIF.

    SORT flights BY airpfrom fldate fltime.
  ENDMETHOD.                    "get_data


  METHOD get_flights_for_single_trip.
    "-----------------------------------------------------------------*
    "   Get flights for a single flight = From dest To dest
    "-----------------------------------------------------------------*
    SELECT * FROM  sflights2
             INTO  TABLE flights
             WHERE airpfrom EQ p_airpfr
               AND airpto   EQ p_airpto
               AND fldate   EQ p_flddep
               AND carrid   IN so_carid
               AND price    IN so_price.

    IF sy-subrc NE 0.
      DATA(flight_date_from) = p_flddep - p_varinc.
      DATA(flight_date_to)   = p_flddep + p_varinc.
      SELECT * FROM  sflights2
               INTO  TABLE flights
               WHERE airpfrom EQ p_airpfr
                 AND airpto   EQ p_airpto
                 AND fldate   BETWEEN flight_date_from AND
                                      flight_date_to
                 AND carrid   IN so_carid
                 AND price    IN so_price.
    ENDIF.
  ENDMETHOD.                    "get_flights_for_single_trip


  METHOD get_flights_for_round_trip.
    "-----------------------------------------------------------------*
    "   Get flights for a round trip
    "-----------------------------------------------------------------*
    get_flights_for_single_trip( ).

    SELECT * FROM  sflights2
             APPENDING TABLE flights
             WHERE airpfrom EQ p_airpto
               AND airpto   EQ p_airpfr
               AND fldate   EQ p_fldret
               AND carrid   IN so_carid
               AND price    IN so_price.

    IF sy-subrc NE 0.
      DATA(flight_date_from) = p_flddep - p_varinc.
      DATA(flight_date_to)   = p_flddep + p_varinc.
      SELECT * FROM  sflights2
               APPENDING TABLE flights
               WHERE airpfrom EQ p_airpto
                 AND airpto   EQ p_airpfr
                 AND fldate   BETWEEN flight_date_from AND
                                      flight_date_to
                 AND carrid   IN so_carid
                 AND price    IN so_price.
    ENDIF.
  ENDMETHOD.                    "get_flights_for_round_trip

ENDCLASS.                     "salv_usage  IMPLEMENTATION
