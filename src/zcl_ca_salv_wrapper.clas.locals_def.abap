*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section



*---------------------------------------------------------------------*
*     CLASS  _access_to_alv_grid  DEFINITION
*---------------------------------------------------------------------*
*     Helper to pick up the CL_GUI_ALV_GRID instance underlying the
*     SALV instance, to be able to provide more functionalities.
*---------------------------------------------------------------------*
CLASS _access_to_alv_grid DEFINITION CREATE PUBLIC
                                            INHERITING FROM cl_salv_model_base
                                            FINAL.
* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Get instance of under laying CL_GUI_ALV_GRID control</p>
      "!
      "! @parameter salv_instance | <p class="shorttext synchronized" lang="en">Current SALV instance</p>
      "! @parameter result        | <p class="shorttext synchronized" lang="en">ALV GRID instance underlying SALV instance</p>
      get_control
        IMPORTING
          salv_instance TYPE REF TO cl_salv_table
        RETURNING
          VALUE(result) TYPE REF TO cl_gui_alv_grid.


* P R I V A T E   S E C T I O N
  PRIVATE SECTION.
*   i n s t a n c e   a t t r i b u t e s
    DATA:
*     o b j e c t   r e f e r e n c e s
      "! <p class="shorttext synchronized" lang="en">Requesting SALV instance</p>
      salv_instance        TYPE REF TO cl_salv_table.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Grab CL_GUI_ALV_GRID control from SALV adapter</p>
      "!
      "! @parameter salv_base | <p class="shorttext synchronized" lang="en">SALV adapter instance</p>
      "! @parameter result    | <p class="shorttext synchronized" lang="en">ALV GRID instance underlying SALV instance</p>
      grab_alv_grid_control_from
        IMPORTING
          salv_base     TYPE REF TO cl_salv_model_base
        RETURNING
          VALUE(result) TYPE REF TO cl_gui_alv_grid,

      "! <p class="shorttext synchronized" lang="en">Check whether the SALV controller is accesible</p>
      "!
      "! @parameter salv_model | <p class="shorttext synchronized" lang="en">SALV model instance</p>
      "! @parameter result     | <p class="shorttext synchronized" lang="en">X = Model instance is already created</p>
      is_adapter_accessible
        IMPORTING
          salv_model    TYPE REF TO cl_salv_model
        RETURNING
          VALUE(result) TYPE abap_boolean,

      "! <p class="shorttext synchronized" lang="en">Check whether the SALV instance is bound</p>
      "!
      "! @parameter salv_instance | <p class="shorttext synchronized" lang="en">Current SALV instance</p>
      is_provided
        IMPORTING
          salv_instance TYPE REF TO cl_salv_table.

ENDCLASS.                    "_access_to_alv_grid  DEFINITION
