"! <p class="shorttext synchronized" lang="en">CA-TBX: Constants n value checks for Top-of-Page in SALV Wr.</p>
CLASS zcl_ca_c_salv_wrapper_top DEFINITION PUBLIC
                                           FINAL
                                           CREATE PRIVATE.
* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   c o n s t a n t s
    CONSTANTS:
      "! <p class="shorttext synchronized" lang="en">Top-of-Page (TOP) element Id</p>
      BEGIN OF element_id,
        "! <p class="shorttext synchronized" lang="en">TOP element Id: List header</p>
        header     TYPE int1 VALUE 1 ##no_text,
        "! <p class="shorttext synchronized" lang="en">TOP element Id: Selections</p>
        selections TYPE int1 VALUE 2 ##no_text,
      END OF element_id,

      "! <p class="shorttext synchronized" lang="en">Top-of-Page (TOP) format type</p>
      BEGIN OF format_type,
        "! <p class="shorttext synchronized" lang="en">TOP format type: List header only</p>
        header_only         TYPE int1 VALUE 1 ##no_text,
        "! <p class="shorttext synchronized" lang="en">TOP format type: Selections only</p>
        selections_only     TYPE int1 VALUE 2 ##no_text,
        "! <p class="shorttext synchronized" lang="en">TOP format type: List header and selections</p>
        header_n_selections TYPE int1 VALUE 3 ##no_text,
      END OF format_type.

*   s t a t i c   m e t h o d s
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Get instance</p>
      "!
      "! @parameter result | <p class="shorttext synchronized" lang="en">Class instance</p>
      get_instance
        RETURNING
          VALUE(result) TYPE REF TO zcl_ca_c_salv_wrapper_top.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Valid TOP element Id passed?</p>
      "!
      "! @parameter top_element_id      | <p class="shorttext synchronized" lang="en">TOP element Id</p>
      "! @raising   zcx_ca_salv_wrapper | <p class="shorttext synchronized" lang="en">Common exception: SALV Wrapper exceptions</p>
      is_top_element_id_valid
        IMPORTING
          top_element_id TYPE int1
        RAISING
          zcx_ca_salv_wrapper,

      "! <p class="shorttext synchronized" lang="en">Valid TOP format type passed?</p>
      "!
      "! @parameter top_format_type     | <p class="shorttext synchronized" lang="en">TOP format type</p>
      "! @raising   zcx_ca_salv_wrapper | <p class="shorttext synchronized" lang="en">Common exception: SALV Wrapper exceptions</p>
      is_top_format_type_valid
        IMPORTING
          top_format_type TYPE int1
        RAISING
          zcx_ca_salv_wrapper.


* P R I V A T E   S E C T I O N
  PRIVATE SECTION.
*   s t a t i c   a t t r i b u t e s
    CLASS-DATA:
*     o b j e c t   r e f e r e n c e s
      "! <p class="shorttext synchronized" lang="en">Instance of the class itself</p>
      singleton_instance     TYPE REF TO zcl_ca_c_salv_wrapper_top.

ENDCLASS.



CLASS zcl_ca_c_salv_wrapper_top IMPLEMENTATION.

  METHOD get_instance.
    "-----------------------------------------------------------------*
    "   Get instance
    "-----------------------------------------------------------------*
    IF zcl_ca_c_salv_wrapper_top=>singleton_instance IS NOT BOUND.
      zcl_ca_c_salv_wrapper_top=>singleton_instance = NEW #( ).
    ENDIF.

    result = zcl_ca_c_salv_wrapper_top=>singleton_instance.
  ENDMETHOD.                    "get_instance


  METHOD is_top_element_id_valid.
    "-----------------------------------------------------------------*
    "   Valid TOP element Id passed?
    "-----------------------------------------------------------------*
    IF top_element_id NOT BETWEEN 1 AND 2.
      "Parameter '&1' has invalid value '&2'
      RAISE EXCEPTION TYPE zcx_ca_salv_wrapper
        EXPORTING
          textid   = zcx_ca_salv_wrapper=>param_invalid
          mv_msgty = 'E'
          mv_msgv1 = 'TOP_ELEMENT_ID'
          mv_msgv2 = CONV #( top_element_id ) ##no_text.
    ENDIF.
  ENDMETHOD.                    "is_approval_result_valid


  METHOD is_top_format_type_valid.
    "-----------------------------------------------------------------*
    "   Valid TOP format type passed?
    "-----------------------------------------------------------------*
    IF top_format_type NOT BETWEEN 1 AND 3.
      "Parameter '&1' has invalid value '&2'
      RAISE EXCEPTION TYPE zcx_ca_salv_wrapper
        EXPORTING
          textid   = zcx_ca_salv_wrapper=>param_invalid
          mv_msgty = 'E'
          mv_msgv1 = 'TOP_FORMAT_TYPE'
          mv_msgv2 = CONV #( top_format_type ) ##no_text.
    ENDIF.
  ENDMETHOD.                    "is_approval_result_valid

ENDCLASS.
