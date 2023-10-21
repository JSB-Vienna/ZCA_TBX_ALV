*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations



*---------------------------------------------------------------------*
*     CLASS  _access_to_alv_grid  IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS _access_to_alv_grid IMPLEMENTATION.

  METHOD get_control.
    "-----------------------------------------------------------------*
    "   Get instance of underlying CL_GUI_ALV_GRID control
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      _extended_grid_api   TYPE REF TO cl_salv_gui_om_table_info.

    is_provided( salv_instance ).

    _extended_grid_api ?= salv_instance->if_salv_gui_om_table_info~extended_grid_api( ).
    result = grab_alv_grid_control_from( _extended_grid_api ).
  ENDMETHOD.                    "get_control


  METHOD grab_alv_grid_control_from.
    "-----------------------------------------------------------------*
    "   Grab CL_GUI_ALV_GRID control from SALV adapter
    "-----------------------------------------------------------------*
    TRY.
        IF NOT is_adapter_accessible( salv_base ).
          salv_instance->display( ).                   "Try to create the adapter instance by myself and ...
          is_adapter_accessible( salv_base ).          "... check again -> if not raise an exception
        ENDIF.

        IF salv_base->r_controller->r_adapter IS INSTANCE OF if_salv_table_display_adapter.
          result = CAST if_salv_table_display_adapter( salv_base->r_controller->r_adapter )->get_grid( ).

        ELSE.
          "SALV adapter is not of type IF_SALV_TABLE_DISPLAY_ADAPTER
          RAISE EXCEPTION TYPE zcx_ca_salv_wrapper
            MESSAGE ID '38' TYPE 'E' NUMBER '001'
            WITH 'SALV adapter is not of type' 'IF_SALV_TABLE_DISPLAY_ADAPTER' ##no_text.
        ENDIF.

      CATCH cx_sy_move_cast_error INTO DATA(_catched).
        DATA(_exception) = CAST zcx_ca_intern( zcx_ca_intern=>create_exception(
                                                             iv_excp_cls = zcx_ca_intern=>c_zcx_ca_intern
                                                             iv_class    = 'LCL_SALV_MODEL'
                                                             iv_method   = 'GRAB_ALV_GRID_CONTROL_FROM'
                                                             ix_error    = _catched ) )  ##no_text.
        IF _exception IS BOUND.
          RAISE EXCEPTION _exception.
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    "grab_alv_grid_control_from


  METHOD is_adapter_accessible.
    "-----------------------------------------------------------------*
    "   Check whether the SALV controller is accessible
    "-----------------------------------------------------------------*
    result = abap_false.
    IF salv_model->r_controller            IS BOUND AND
       salv_model->r_controller->r_adapter IS BOUND.
      result = abap_true.
    ENDIF.

    IF result IS NOT SUPPLIED AND
       result EQ abap_false.
      "SALV adapter is not supplied before method SALV->DISPLAY was called
      RAISE EXCEPTION TYPE zcx_ca_salv_wrapper
        EXPORTING
          textid = zcx_ca_salv_wrapper=>adapter_not_bound.
    ENDIF.
  ENDMETHOD.                    "is_adapter_accessible


  METHOD is_provided.
    "-----------------------------------------------------------------*
    "   Check whether the SALV instance is bound
    "-----------------------------------------------------------------*
    IF salv_instance IS NOT BOUND.
      "Parameter '&1' is not specified
      RAISE EXCEPTION TYPE zcx_ca_salv_wrapper
        EXPORTING
          textid   = zcx_ca_salv_wrapper=>param_not_supplied
          mv_msgty = zcx_ca_salv_wrapper=>c_msgty_e
          mv_msgv1 = 'SALV_INSTANCE is not bound' ##no_text.
    ENDIF.

    me->salv_instance = salv_instance.
  ENDMETHOD.                    "is_provided

ENDCLASS.                    "_access_to_alv_grid  IMPLEMENTATION
