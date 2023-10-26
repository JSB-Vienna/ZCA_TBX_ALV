REPORT zca_demo_salv_wrapper.

* t a b l e s   /   s t r u c t u r e s   for selection field definition
TABLES:
  sflights2.  "View over SCARR, SPFLI and SFLIGHT


* s e l e c t i o n   f i e l d s
*- Data selection ----------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK data_selection WITH FRAME TITLE TEXT-bsl.
*- Choose flight connections -----------------------------------------*
  SELECTION-SCREEN BEGIN OF BLOCK connections WITH FRAME TITLE TEXT-bcn.
    SELECTION-SCREEN SKIP.

    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 01(20) FOR FIELD p_airpfr.
      SELECTION-SCREEN COMMENT 40(20) FOR FIELD p_airpto.
    SELECTION-SCREEN END OF LINE.

    SELECTION-SCREEN BEGIN OF LINE.
      PARAMETERS
        "Departing from
        p_airpfr       TYPE s_fromairp     VISIBLE LENGTH 30
                                           AS LISTBOX
                                           USER-COMMAND airport_from_sel.
      SELECTION-SCREEN POSITION 40.
      PARAMETERS
        "Arriving at
        p_airpto       TYPE S_toairp       VISIBLE LENGTH 30
                                           AS LISTBOX.
    SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN END OF BLOCK connections.

  SELECTION-SCREEN SKIP.

*- Choose travel dates -----------------------------------------------*
  SELECTION-SCREEN BEGIN OF BLOCK travel_dates WITH FRAME TITLE TEXT-btd.
    SELECTION-SCREEN BEGIN OF LINE.
      PARAMETERS
        "Trip type
        p_tripty       TYPE zca_d_demo_trip_type VISIBLE LENGTH 30
                                                 AS LISTBOX
                                                 DEFAULT 'R'
                                                 USER-COMMAND trip_type_selected.
    SELECTION-SCREEN END OF LINE.

    SELECTION-SCREEN SKIP.
    "Currently available flight dates are between dd.mm.yyyy and dd.mm.yyyy
    SELECTION-SCREEN COMMENT /1(75) txcm_tdd.

    SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 01(19) FOR FIELD p_flddep.
      SELECTION-SCREEN POSITION 21.
      PARAMETERS
        "Departure date
        p_flddep       TYPE s_date.

      SELECTION-SCREEN COMMENT 40(20) FOR FIELD p_fldret MODIF ID ret.
      SELECTION-SCREEN POSITION 62.
      PARAMETERS
        "Return flight on
        p_fldret       TYPE s_date  MODIF ID ret.
    SELECTION-SCREEN END OF LINE.

    SELECTION-SCREEN SKIP.
    SELECTION-SCREEN COMMENT /01(75) TEXT-tdv.
    PARAMETERS
      "Variance of +/- n days
      p_varinc         TYPE int1 DEFAULT 1.
  SELECTION-SCREEN END OF BLOCK travel_dates.

  SELECTION-SCREEN SKIP.

*- Additional flight selections --------------------------------------*
  SELECTION-SCREEN BEGIN OF BLOCK additional_sels WITH FRAME TITLE TEXT-bas.
    SELECT-OPTIONS:
      "Airline
      so_carid       FOR  sflights2-carrid,
      "Price
      so_price       FOR  sflights2-price.
  SELECTION-SCREEN END OF BLOCK additional_sels.
SELECTION-SCREEN END OF BLOCK data_selection.

SELECTION-SCREEN SKIP.


"Selection screen block for flight model data creation incl. class for control
INCLUDE zca_demo_selscr_flight_model ##incl_ok.


CLASS search_flight_connections DEFINITION DEFERRED.


INCLUDE zca_demo_salv_wrapperd01.    "Local class definition: Selection screen handler
INCLUDE zca_demo_salv_wrapperd02.    "Local class definition: Demo using SALV wrapper


INCLUDE zca_demo_salv_wrapperp01.    "Local class implementation: Selection screen handler
INCLUDE zca_demo_salv_wrapperp02.    "Local class implementation: Demo using SALV wrapper


*---------------------------------------------------------------------*
*     i n i t i a l i z a t i o n
*---------------------------------------------------------------------*
INITIALIZATION.
  DATA(flight_search) = NEW search_flight_connections( ).


*---------------------------------------------------------------------*
*     a t   s e l e c t i o n - s c r e e n   OUTPUT
*---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
  flight_search->sel_screen->at_sel_screen_output( ).


*---------------------------------------------------------------------*
*     a t   s e l e c t i o n - s c r e e n   o n   BLOCK
*---------------------------------------------------------------------*
AT SELECTION-SCREEN ON BLOCK connections.
  flight_search->sel_screen->at_sel_screen_block_conns( ).


AT SELECTION-SCREEN ON BLOCK travel_dates.
  flight_search->sel_screen->at_sel_screen_block_dates( ).


*---------------------------------------------------------------------*
*     a t   s e l e c t i o n - s c r e e n
*---------------------------------------------------------------------*
AT SELECTION-SCREEN.
  flight_search->sel_screen->at_sel_screen( ).


*---------------------------------------------------------------------*
*     s t a r t - o f - s e l e c t i o n
*---------------------------------------------------------------------*
START-OF-SELECTION.
  flight_search->process( ).
