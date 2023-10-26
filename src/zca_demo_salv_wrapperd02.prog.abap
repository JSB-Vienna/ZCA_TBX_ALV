"! <p class="shorttext synchronized" lang="en">Demo using SALV wrapper: Search flight connections</p>
CLASS search_flight_connections DEFINITION FINAL
                                INHERITING FROM zcl_ca_salv_wrapper
                                CREATE PUBLIC.
* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   c o n s t a n t s
    CONSTANTS:
      "! <p class="shorttext synchronized" lang="en">Trip types</p>
      BEGIN OF trip_type,
        "! <p class="shorttext synchronized" lang="en">Trip type: Multiple connections</p>
        multiple TYPE zca_d_demo_trip_type VALUE 'M'  ##no_text,
        "! <p class="shorttext synchronized" lang="en">Trip type: Round trip</p>
        round    TYPE zca_d_demo_trip_type VALUE 'R'  ##no_text,
        "! <p class="shorttext synchronized" lang="en">Trip type: Single flight</p>
        single   TYPE zca_d_demo_trip_type VALUE 'S'  ##no_text,
      END OF trip_type.

*   i n s t a n c e   a t t r i b u t e s
    DATA:
*     o b j e c t   r e f e r e n c e s
      "! <p class="shorttext synchronized" lang="en">Selection screen handler</p>
      sel_screen   TYPE REF TO sel_screen_handler.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Constructor</p>
      constructor,

      process REDEFINITION.


* P R O T E C T E D   S E C T I O N
  PROTECTED SECTION.
*   i n s t a n c e   m e t h o d s
    METHODS:
      prepare_alv REDEFINITION.


* P R I V A T E   S E C T I O N
  PRIVATE SECTION.
*   i n s t a n c e   a t t r i b u t e s
    DATA:
*     o b j e c t   r e f e r e n c e s
*      "! <p class="shorttext synchronized" lang="en">Description</p>
*      mo_...               TYPE REF TO x..
*
**     d a t a   r e f e r e n c e s
*      "! <p class="shorttext synchronized" lang="en">Description</p>
*      mr_...               TYPE REF TO x..

*     t a b l e s
      "! <p class="shorttext synchronized" lang="en">Selected connections</p>
      flights      TYPE STANDARD TABLE OF sflights2.
*      "! <p class="shorttext synchronized" lang="en">Airports for value listbox</p>
*      airports     TYPE vrm_values.

**     s t r u c t u r e s
*      "! <p class="shorttext synchronized" lang="en">Description</p>
*      ms_...               TYPE x..
*
**     s i n g l e   v a l u e s
*      "! <p class="shorttext synchronized" lang="en">Description</p>
*      mv_...               TYPE x..

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Get data from DB respecting selections</p>
      get_data
        RAISING
          zcx_ca_dbacc,

      "! <p class="shorttext synchronized" lang="en">Get flights for a single flight</p>
      get_flights_for_single_trip,

      "! <p class="shorttext synchronized" lang="en">Get flights for a round trip</p>
      get_flights_for_round_trip.

ENDCLASS.                     "salv_usage  DEFINITION
