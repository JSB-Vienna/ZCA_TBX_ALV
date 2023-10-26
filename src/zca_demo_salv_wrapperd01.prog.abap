"! <p class="shorttext synchronized" lang="en">Selection screen handler</p>
CLASS sel_screen_handler DEFINITION FINAL
                                    CREATE PUBLIC.
* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   i n t e r f a c e s
    INTERFACES:
      if_xo_const_message.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Constructor</p>
      constructor
        IMPORTING
          io_flight_search TYPE REF TO search_flight_connections,

      "! <p class="shorttext synchronized" lang="en">Control / adjust selection screen fields</p>
      at_sel_screen_output,

      "! <p class="shorttext synchronized" lang="en">Check selection values on block 'Connections'</p>
      at_sel_screen_block_conns,

      "! <p class="shorttext synchronized" lang="en">Check selection values on block 'Travel dates'</p>
      at_sel_screen_block_dates,

      "! <p class="shorttext synchronized" lang="en">Check selection values and handle user commands</p>
      at_sel_screen.


* P R I V A T E   S E C T I O N
  PRIVATE SECTION.
*   a l i a s e s
    ALIASES:
*     Message types
      msg_type_e           FOR  if_xo_const_message~error,
      msg_type_i           FOR  if_xo_const_message~info,
      msg_type_s           FOR  if_xo_const_message~success,
      msg_type_w           FOR  if_xo_const_message~warning.

*   i n s t a n c e   a t t r i b u t e s
    DATA:
*     o b j e c t   r e f e r e n c e s
      "! <p class="shorttext synchronized" lang="en">Demo using SALV wrapper: Search flight connections</p>
      parent        TYPE REF TO search_flight_connections,
      "! <p class="shorttext synchronized" lang="en">Screen field attributes (usage with table SCREEN)</p>
      scr_fld_attr  TYPE REF TO zcl_ca_c_screen_field_attr,
      "! <p class="shorttext synchronized" lang="en">(Re-)Create flight model data for demo programs</p>
      flight_model  TYPE REF TO flight_model,

**     d a t a   r e f e r e n c e s
*      "! <p class="shorttext synchronized" lang="en">Description</p>
*      mr_...               TYPE REF TO x..

*     t a b l e s
      "! <p class="shorttext synchronized" lang="en">From-Airports for value listbox</p>
      airports_from TYPE vrm_values,
      "! <p class="shorttext synchronized" lang="en">To-Airports for value listbox</p>
      airports_to   TYPE vrm_values,

*     s t r u c t u r e s
      "! <p class="shorttext synchronized" lang="en">Available flight date range in DB table SFLIGHT</p>
      BEGIN OF flight_date_range,
        lowest  TYPE s_fldate1,
        highest TYPE s_fldate1,
      END   OF flight_date_range.

**     s i n g l e   v a l u e s
*      "! <p class="shorttext synchronized" lang="en"></p>
*      flight_date_min      TYPE s_fldate1,

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Prepare airport names for listbox</p>
      get_available_flightdate_range,

      "! <p class="shorttext synchronized" lang="en">Prepare airport names for listbox</p>
      get_airports_for_value_req,

      "! <p class="shorttext synchronized" lang="en">Set default selection values</p>
      set_default_values,

      "! <p class="shorttext synchronized" lang="en">Set airport values for listbox</p>
      set_airports_for_listbox,

      "! <p class="shorttext synchronized" lang="en">Adjust selection screen to current settings</p>
      adjust_selection_screen,

      "! <p class="shorttext synchronized" lang="en">Check entered selections</p>
      "!
      "! @raising   zcx_ca_param | <p class="shorttext synchronized" lang="en">x</p>
      check_selection_input
        RAISING
          zcx_ca_param.
ENDCLASS.                     "sel_screen_handler  DEFINITION
