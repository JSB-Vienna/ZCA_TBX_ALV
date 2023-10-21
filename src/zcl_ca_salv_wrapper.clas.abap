"! <p class="shorttext synchronized" lang="en">CA-TBX: Abstract SALV wrapper</p>
CLASS zcl_ca_salv_wrapper DEFINITION PUBLIC
                                     ABSTRACT
                                     CREATE PUBLIC.

* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   i n t e r f a c e s
    INTERFACES:
      if_xo_const_message,
      if_salv_c_alignment,
      if_salv_form_c_h_align.

*   a l i a s e s
    ALIASES:
*     Message types
      c_msgty_e            FOR if_xo_const_message~error,
      c_msgty_i            FOR if_xo_const_message~info,
      c_msgty_s            FOR if_xo_const_message~success,
      c_msgty_w            FOR if_xo_const_message~warning.

*   i n s t a n c e   a t t r i b u t e s
    DATA:
*     o b j e c t   r e f e r e n c e s
      "! <p class="shorttext synchronized" lang="en">Constants for Top-of-Page in SALV wrapper</p>
      mo_top_options          TYPE REF TO zcl_ca_c_salv_wrapper_top READ-ONLY.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Constructor</p>
      "!
      "! @parameter ir_table              | <p class="shorttext synchronized" lang="en">Data ref. of output table (table can be empty at this point)</p>
      "! @parameter iv_prg_title          | <p class="shorttext synchronized" lang="en">Title of the program (will be used in list header)</p>
      "! @parameter iv_list_title         | <p class="shorttext synchronized" lang="en">Short description (is set as ALV header and as 2nd title)</p>
      "! @parameter iv_register_events    | <p class="shorttext synchronized" lang="en">X = Register event methods (not TOP-OF-PAGE)</p>
      "! @parameter iv_layout_restriction | <p class="shorttext synchronized" lang="en">Restriction of saving ALV layouts (use const C_LAYO_RESTR_*)</p>
      "! @parameter iv_prepare_default    | <p class="shorttext synchronized" lang="en">X = Prepare ALV with standard settings + member MT_COLS</p>
      "! @parameter iv_prg_variants       | <p class="shorttext synchronized" lang="en">Program name for saving layout variants</p>
      "! @parameter iv_layout_handle      | <p class="shorttext synchronized" lang="en">ID to differ between different ALV calls in one program</p>
      "! @parameter io_container          | <p class="shorttext synchronized" lang="en">Container instance, if ALV is used inplace with controls</p>
      "! @parameter iv_cnt_name           | <p class="shorttext synchronized" lang="en">Name of the ALV control/container</p>
      constructor
        IMPORTING
          ir_table              TYPE REF TO data
          iv_prg_title          TYPE syst_title                 DEFAULT sy-title
          iv_list_title         TYPE lvc_title                  OPTIONAL
          iv_register_events    TYPE abap_bool                  DEFAULT abap_false
          iv_layout_restriction TYPE salv_de_layout_restriction DEFAULT if_salv_c_layout=>restrict_none
          iv_prepare_default    TYPE abap_bool                  DEFAULT abap_true
          iv_prg_variants       TYPE sycprog                    DEFAULT sy-cprog
          iv_layout_handle      TYPE slis_handl                 OPTIONAL
          io_container          TYPE REF TO cl_gui_container    OPTIONAL
          iv_cnt_name           TYPE csequence                  OPTIONAL,

      "! <p class="shorttext synchronized" lang="en">Controls entire processing (has to be redefined)</p>
      process ABSTRACT,

      "! <p class="shorttext synchronized" lang="en">Handle additional functions (has to be redefined)</p>
      "!
      "! @parameter e_salv_function | <p class="shorttext synchronized" lang="en">Function code</p>
      on_added_function
        FOR EVENT added_function OF cl_salv_events_table
        IMPORTING
          e_salv_function,

      "! <p class="shorttext synchronized" lang="en">Handle double click (has to be redefined)</p>
      "!
      "! @parameter row    | <p class="shorttext synchronized" lang="en">Row index</p>
      "! @parameter column | <p class="shorttext synchronized" lang="en">Technical column name</p>
      on_double_click
        FOR EVENT double_click OF cl_salv_events_table
        IMPORTING
          row
          column,

      "! <p class="shorttext synchronized" lang="en">Handle link click (has to be redefined)</p>
      "!
      "! @parameter row    | <p class="shorttext synchronized" lang="en">Row index</p>
      "! @parameter column | <p class="shorttext synchronized" lang="en">Technical column name</p>
      on_link_click
        FOR EVENT link_click OF cl_salv_events_table
        IMPORTING
          row
          column,

      "! <p class="shorttext synchronized" lang="en">Print output of list header</p>
      "!
      "! This method is called as event handler by SALV for the list header. If the list header should be
      "! enhanced, this method <strong>must be redefined</strong>. Furthermore the list header PRINT output has
      "! to be <strong>activated</strong> by calling method ACTIVATE_TOP <strong>before</strong> method DISPLAY
      "! is executed.
      "!
      "! Please pay also attention to the comments within this or the super method!!
      "!
      "! @parameter r_top_of_page | <p class="shorttext synchronized" lang="en">List header object</p>
      "! @parameter page          | <p class="shorttext synchronized" lang="en">Page number</p>
      "! @parameter table_index   | <p class="shorttext synchronized" lang="en">Table index</p>
      on_top_of_page
        FOR EVENT top_of_page OF cl_salv_events_table
        IMPORTING
          r_top_of_page
          page
          table_index,

      "! <p class="shorttext synchronized" lang="en">Release objects when closing applicaton</p>
      free.

* P R O T E C T E D   S E C T I O N
  PROTECTED SECTION.
*   a l i a s e s
    ALIASES:
      c_align_list_cell_left         FOR  if_salv_c_alignment~left,         "1
      c_align_list_cell_centered     FOR  if_salv_c_alignment~centered,     "3
      c_align_list_cell_right        FOR  if_salv_c_alignment~right,        "2

      c_align_form_cell_left         FOR  if_salv_form_c_h_align~left,      "1
      c_align_form_cell_centered     FOR  if_salv_form_c_h_align~center,    "2
      c_align_form_cell_right        FOR  if_salv_form_c_h_align~right.     "3

*   l o c a l   t y p e   d e f i n i t i o n
    TYPES:
      "! <p class="shorttext synchronized" lang="en">Calculated width of list header columns</p>
      BEGIN OF ty_s_column_width,
        overall TYPE i,
        left    TYPE i,
        center  TYPE i,
        right   TYPE i,
        no_cols TYPE i,
      END   OF ty_s_column_width.

*   i n s t a n c e   a t t r i b u t e s
    DATA:
*     o b j e c t   r e f e r e n c e s
      "! <p class="shorttext synchronized" lang="en">Base class for simple tables</p>
      mo_salv           TYPE REF TO cl_salv_table,
      "! <p class="shorttext synchronized" lang="en">Reference of the internal data table</p>
      mr_table          TYPE REF TO data,
      "! <p class="shorttext synchronized" lang="en">Technical description of the current class itself</p>
      mo_cls_desc       TYPE REF TO cl_abap_classdescr,
      "! <p class="shorttext synchronized" lang="en">Parentcontainer of the SALV</p>
      mo_container      TYPE REF TO cl_gui_container,
      "! <p class="shorttext synchronized" lang="en">ALV_GRID Instance behind current SALV for additional funct.</p>
      mo_alv_grid       TYPE REF TO cl_gui_alv_grid,
      "! <p class="shorttext synchronized" lang="en">List header grid layout object (normally with 3 rows+3 cols)</p>
      mo_top            TYPE REF TO cl_salv_form_layout_grid,
      "! <p class="shorttext synchronized" lang="en">Function code constants (with suggested icons)</p>
      mo_fcodes         TYPE REF TO zcl_ca_c_fcodes,

*     t a b l e s
      "! <p class="shorttext synchronized" lang="en">ALV list columns (only set when IV_PREPARE_DEFAULT = X)</p>
      mt_cols           TYPE salv_t_column_ref,

*     s t r u c t u r e s
      "! <p class="shorttext synchronized" lang="en">Calculated width of list header columns</p>
      ms_col_width      TYPE ty_s_column_width,
      "! <p class="shorttext synchronized" lang="en">Selection fields to be hidden</p>
      ms_hide_sels      TYPE slis_sel_hide_alv,

*     s i n g l e   v a l u e s
      "! <p class="shorttext synchronized" lang="en">1=List hdr, 2=Sel, 3=List hdr+sel (see const C_HDR_FORMAT_*)</p>
      mv_header_format  TYPE i,
      "! <p class="shorttext synchronized" lang="en">Short description/purpose of the list (as second title line)</p>
      mv_list_title     TYPE lvc_title,
      "! <p class="shorttext synchronized" lang="en">Title of the program</p>
      mv_prg_title      TYPE sytitle,
      "! <p class="shorttext synchronized" lang="en">Program name for saving layout variants</p>
      mv_prg_variants   TYPE sycprog,
      "! <p class="shorttext synchronized" lang="en">X = List header is activated</p>
      mv_top_activated  TYPE abap_bool,
      "! <p class="shorttext synchronized" lang="en">X = Header area for selections is already prepared</p>
      mv_top_sel_crea   TYPE abap_bool,
      "! <p class="shorttext synchronized" lang="en">X = Only output of selections with values</p>
      mv_used_sels_only TYPE abap_bool.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Activation of list header per page for printing</p>
      "!
      "! @parameter io_top              | <p class="shorttext synchronized" lang="en">Adapted or individual list header object</p>
      "! @parameter iv_header_format    | <p class="shorttext synchronized" lang="en">1=List hdr, 2=Sel, 3=List hdr+sel (use const C_HDR_FORMAT_*)</p>
      "! @parameter iv_used_sels_only   | <p class="shorttext synchronized" lang="en">X = Display only selections with values</p>
      "! @parameter is_hide_sels        | <p class="shorttext synchronized" lang="en">Selection fields to be hidden</p>
      "! @parameter iv_act_for_printing | <p class="shorttext synchronized" lang="en">X = Activate for printing</p>
      activate_top
        IMPORTING
          io_top              TYPE REF TO cl_salv_form_element OPTIONAL
          iv_header_format    TYPE int1                        DEFAULT zcl_ca_c_salv_wrapper_top=>format_type-header_only
          iv_used_sels_only   TYPE abap_bool                   DEFAULT abap_true
          is_hide_sels        TYPE slis_sel_hide_alv           OPTIONAL
          iv_act_for_printing TYPE abap_bool                   DEFAULT abap_true,

      "! <p class="shorttext synchronized" lang="en">Align text in layout column of list header</p>
      "!
      "! @parameter iv_text  | <p class="shorttext synchronized" lang="en">Text to be aligned</p>
      "! @parameter iv_align | <p class="shorttext synchronized" lang="en">Alignment (use const MC_ALIGN_*)</p>
      "! @parameter rv_text  | <p class="shorttext synchronized" lang="en">Aligned text</p>
      align_text
        IMPORTING
          iv_text        TYPE csequence
          iv_align       TYPE i
        RETURNING
          VALUE(rv_text) TYPE string,

      "! <p class="shorttext synchronized" lang="en">List header columns: Calculate width and align</p>
      "!
      "! @parameter io_top          | <p class="shorttext synchronized" lang="en">Grid layout object for Top-of-Page (TOP)</p>
      "! @parameter rs_column_width | <p class="shorttext synchronized" lang="en">Calculated width for output list header</p>
      calc_n_set_width_n_alignm_hdr
        IMPORTING
          io_top                 TYPE REF TO cl_salv_form_layout_grid
        RETURNING
          VALUE(rs_column_width) TYPE ty_s_column_width,

      "! <p class="shorttext synchronized" lang="en">List header columns: Calculate width for selections</p>
      "!
      "! @parameter io_top          | <p class="shorttext synchronized" lang="en">Grid layout object for Top-of-Page (TOP)</p>
      "! @parameter rs_column_width | <p class="shorttext synchronized" lang="en">Calculated width for output selections</p>
      calc_n_set_width_sel
        IMPORTING
          io_top                 TYPE REF TO cl_salv_form_layout_grid
        RETURNING
          VALUE(rs_column_width) TYPE ty_s_column_width,

      "! <p class="shorttext synchronized" lang="en">Create list header</p>
      "!
      "! @parameter iv_page | <p class="shorttext synchronized" lang="en">Current page number (only during printing)</p>
      "! @parameter ro_top  | <p class="shorttext synchronized" lang="en">Grid layout object for Top-of-Page(TOP)(with 3 rows+3 cols)</p>
      create_top_header
        IMPORTING
          iv_page       TYPE sypagno
        RETURNING
          VALUE(ro_top) TYPE REF TO cl_salv_form_layout_grid,

      "! <p class="shorttext synchronized" lang="en">Create list header area with selections</p>
      create_top_selections,

      "! <p class="shorttext synchronized" lang="en">Get instance of ALV_GRID for additional functionalities</p>
      "!
      "! <p>Use this method to get access to the underlying ALV_GRID control to have access at additional
      "! functionalities, like e. g. set a row as first row in the display area. After executing this method the
      "! control is provided within the attribute MO_ALV_GRID.</p>
      "! <p>The ALV_GRID control is soonest be accessible after the execution of method MO_SALV->DISPLAY. The
      "! execution is also tried by this method.</p>
      "! <p>Since the method MO_SALV->DISPLAY will not be left until the user is leaving the program, an explicit
      "! function is necessary to be implemented and has to be used by the user, e. g. by a toggle function between
      "! display and change mode. The trick, to execute simply a WRITE after MO_SALV->DISPLAY, doesn't work
      "! by my experiences.</p>
      "! <p>
      grab_underlying_alv_grid_ctrl,

      "! <p class="shorttext synchronized" lang="en">Get / create layout object for list header</p>
      "!
      "! @parameter iv_element_id  | <p class="shorttext synchronized" lang="en">1=List header, 2=Selections (see const C_TOP_ELEM_*)</p>
      "! @parameter ro_top_element | <p class="shorttext synchronized" lang="en">Grid layout object for list header / selections</p>
      get_top_element
        IMPORTING
          iv_element_id         TYPE int1 DEFAULT zcl_ca_c_salv_wrapper_top=>element_id-header
        RETURNING
          VALUE(ro_top_element) TYPE REF TO cl_salv_form_layout_grid,

      "! <p class="shorttext synchronized" lang="en">Other ALV preparations (has to be redefined)</p>
      "!
      "! @raising cx_salv_error | <p class="shorttext synchronized" lang="en">ALV: General Error Class (Checked During Syntax Check)</p>
      "! @raising zcx_ca_param  | <p class="shorttext synchronized" lang="en">Common exception: Parameter error (INHERIT from this excep!)</p>
      prepare_alv
        RAISING
          cx_salv_error
          zcx_ca_param,

      "! <p class="shorttext synchronized" lang="en">Do default preparations</p>
      prepare_default,

      "! <p class="shorttext synchronized" lang="en">Set individual GUI status</p>
      "!
      "! @parameter iv_prg_status | <p class="shorttext synchronized" lang="en">Name of the program the status is defined for</p>
      "! @parameter iv_pfstatus   | <p class="shorttext synchronized" lang="en">Individual status (copied from FG SALV_METADATA_STATUS)</p>
      "! @parameter iv_functions  | <p class="shorttext synchronized" lang="en">Activating the SALV Function groups (use const C_FUNCTION_*)</p>
      set_status
        IMPORTING
          iv_prg_status TYPE sycprog          DEFAULT sy-cprog
          iv_pfstatus   TYPE sypfkey
          iv_functions  TYPE salv_de_constant DEFAULT cl_salv_table=>c_functions_all.
ENDCLASS.



CLASS zcl_ca_salv_wrapper IMPLEMENTATION.

  METHOD activate_top.
    "-----------------------------------------------------------------*
    "   Activate list header per page for print output
    "-----------------------------------------------------------------*
    mv_header_format  = iv_header_format.
    mv_used_sels_only = iv_used_sels_only.
    ms_hide_sels      = is_hide_sels.

    "Keep TOP-Element if supplied
    IF io_top IS BOUND.
      mo_top ?= io_top.

    ELSE.
      "Create TOP element depending on passed format
      CASE iv_header_format.
        WHEN mo_top_options->format_type-header_only.
          create_top_header( 1 ).

        WHEN mo_top_options->format_type-selections_only.
          create_top_selections( ).

        WHEN mo_top_options->format_type-header_n_selections.
          create_top_header( 1 ).
          create_top_selections( ).
      ENDCASE.
    ENDIF.

    "Is it a grid check number of lines -> the created TOL element
    "by this class is always a grid element
    IF mo_top->get_row_count( ) GT 0.
      mo_salv->set_top_of_list( mo_top ).
    ENDIF.

    "To avoid multiple printing of the TOP-OF-PAGE area check flag MV_TOP_ACTIVATED
    IF   mv_top_activated     NE abap_true   AND
       ( iv_act_for_printing  EQ abap_true    OR
         sy-batch             EQ abap_true ).
      "Register for event Top-of-page
      DATA(lo_event) = mo_salv->get_event( ).
      SET HANDLER on_top_of_page FOR lo_event.
      mv_top_activated = abap_true.
    ENDIF.
  ENDMETHOD.                    "activate_top


  METHOD align_text.
    "-----------------------------------------------------------------*
    "   Arrange text in layout column of the list header
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lv_text               TYPE bapi_msg.

    lv_text = iv_text.

    CASE iv_align.
      WHEN c_align_form_cell_left.
        "Nothing to do - standard

      WHEN c_align_form_cell_centered.
        IF ms_col_width-center GT numofchar( iv_text ).
          SHIFT lv_text BY ( ms_col_width-center  DIV 2 ) -
                           ( numofchar( iv_text ) DIV 2 ) PLACES RIGHT.
        ENDIF.

      WHEN c_align_form_cell_right.
        IF ms_col_width-right GT numofchar( iv_text ).
          SHIFT lv_text BY ms_col_width-right - numofchar( lv_text ) PLACES RIGHT.
        ENDIF.
    ENDCASE.

    rv_text = lv_text.
  ENDMETHOD.                    "align_text


  METHOD calc_n_set_width_n_alignm_hdr.
    "-----------------------------------------------------------------*
    "   List header columns: Calculate width and arrange
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lo_col                TYPE REF TO cl_salv_form_grid_column.

    rs_column_width-overall = COND #( WHEN sy-linsz EQ 0
                                     THEN sy-scols      "use width of screen
                                     ELSE sy-linsz ).

    rs_column_width-center = rs_column_width-overall DIV 2.

    rs_column_width-left  = rs_column_width-center DIV 2.
    "minus 2 digits for the column delimiter
    rs_column_width-right = rs_column_width-overall - 2 -
                            rs_column_width-center  - rs_column_width-left.

    "Set width of grid
    DATA(lo_layout) = CAST cl_salv_form_layout_data_grid( io_top->get_layout_data( ) ).
    lo_layout->set_width( CONV #( rs_column_width-overall ) ).

    "Adjust each column and set width
    lo_col = io_top->get_column( 1 ).
    lo_col->set_h_align( c_align_form_cell_left ).
    lo_col->set_width( CONV #( rs_column_width-left ) ).

    lo_col = io_top->get_column( 2 ).
    lo_col->set_h_align( c_align_form_cell_centered ).
    lo_col->set_width( CONV #( rs_column_width-center ) ).

    lo_col = io_top->get_column( 3 ).
    lo_col->set_h_align( c_align_form_cell_right ).
    lo_col->set_width( CONV #( rs_column_width-right ) ).
  ENDMETHOD.                    "calc_n_set_width_n_alignm_hdr


  METHOD calc_n_set_width_sel.
    "-----------------------------------------------------------------*
    "   Output selections: Calculate width of the columns
    "-----------------------------------------------------------------*
    "Determine overall width
    rs_column_width-overall = COND #( WHEN sy-linsz EQ 0
                                        THEN sy-scols      "use width of screen
                                        ELSE sy-linsz ).   "else printing line-size

    "Set number of columns depending on overall width
    rs_column_width-no_cols = COND #( WHEN rs_column_width-overall LE 60
                                        THEN 2
                                      WHEN rs_column_width-overall BETWEEN 61 AND 100
                                        THEN 3
                                      WHEN rs_column_width-overall BETWEEN 101 AND 140
                                        THEN 4
                                      WHEN rs_column_width-overall BETWEEN 141 AND 180
                                        THEN 5
                                      WHEN rs_column_width-overall GT 180
                                        THEN 7 ).

    "Set number of columns for this element
    io_top->set_column_count( rs_column_width-no_cols ).

    "Set width of grid
    DATA(lo_layout) = CAST cl_salv_form_layout_data_grid( io_top->get_layout_data( ) ).
    lo_layout->set_width( CONV #( rs_column_width-overall ) ).

    "Left width minus 1 digit for each of the column delimiter is the width
    "for each column
    rs_column_width-left = ( rs_column_width-overall DIV rs_column_width-no_cols ) - 1.

    "Adjust each column and set width
    DO rs_column_width-no_cols TIMES.
      DATA(lo_col) = CAST cl_salv_form_grid_column( io_top->get_column( sy-index ) ).
      lo_layout->set_width( CONV #( rs_column_width-left ) ).
    ENDDO.
  ENDMETHOD.                    "calc_n_set_width_sel


  METHOD constructor.
    "-----------------------------------------------------------------*
    "   Constructor
    "-----------------------------------------------------------------*
    TRY.
        "Provide constants
        mo_fcodes      = zcl_ca_c_fcodes=>get_instance( ).
        mo_top_options = zcl_ca_c_salv_wrapper_top=>get_instance( ).

        "Get description of current instance to specific properties
        IF mo_cls_desc IS NOT BOUND.
          mo_cls_desc ?= cl_abap_classdescr=>describe_by_object_ref( me ).
        ENDIF.

        "Dereference table for factory call
        mr_table = ir_table.
        ASSIGN mr_table->* TO FIELD-SYMBOL(<lt_table>).

        "Create ALV instance
        IF io_container IS BOUND.
          "ALV should be displayed within a GUI container
          mo_container = io_container.
          zcl_ca_cfw_util=>set_name( io_control  = mo_container
                                     iv_cnt_name = iv_cnt_name ).

          cl_salv_table=>factory(
                            EXPORTING
                              r_container    = io_container
                              container_name = CONV string( iv_cnt_name )
                            IMPORTING
                              r_salv_table   = mo_salv
                            CHANGING
                              t_table        = <lt_table> ).

        ELSE.
          "ALV should be displayed in a full screen
          cl_salv_table=>factory(
                            IMPORTING
                              r_salv_table = mo_salv
                            CHANGING
                              t_table      = <lt_table> ).

        ENDIF.

        "Program name for saving variants and output of selections values in the header
        mv_prg_variants = iv_prg_variants.
        mv_prg_title    = iv_prg_title.     "Program title
        mv_list_title   = iv_list_title.    "Set list title for list header
        IF mv_list_title IS INITIAL.
          mv_list_title = mv_prg_title.
        ENDIF.

        "Set layout values
        mo_salv->get_layout( )->set_key( VALUE #( report = mv_prg_variants
                                                  handle = iv_layout_handle ) ).
        mo_salv->get_layout( )->set_save_restriction( iv_layout_restriction ).

        "Preparing default settings for ALV, if requested
        IF iv_prepare_default EQ abap_true.
          prepare_default( ).
        ENDIF.

        "Register methods of event handler
        IF iv_register_events EQ abap_true.
          DATA(lo_event) = mo_salv->get_event( ).
          SET HANDLER: on_added_function FOR lo_event,
                       on_double_click   FOR lo_event,
                       on_link_click     FOR lo_event.
        ENDIF.

      CATCH cx_root INTO DATA(lx_salv_err).
        DATA(lx_error) =
              CAST zcx_ca_salv_wrapper(
                       zcx_ca_intern=>create_exception(
                                          iv_excp_cls = zcx_ca_salv_wrapper=>c_zcx_ca_salv_wrapper
                                          iv_class    = 'CL_SALV_TABLE'
                                          iv_method   = 'FACTORY'
                                          ix_error    = lx_salv_err ) )  ##no_text.
        IF lx_error IS BOUND.
          RAISE EXCEPTION lx_error.
        ENDIF.
    ENDTRY.
  ENDMETHOD.                    "constructor


  METHOD create_top_header.
    "-----------------------------------------------------------------*
    "   Create list header
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lv_date  TYPE char15,
      lv_timec TYPE char10.

    "Create grid layout with 3 columns for Top-Of-Page
    DATA(lo_header) = get_top_element( ).

    "C a l c u l a t e   w i d t h   on base of line-size
    ms_col_width = calc_n_set_width_n_alignm_hdr( lo_header ).

    "* - * - * - * - * - * - *   L I N E  1   * - * - * - * - * - * - *
    "Report id.
    lo_header->create_text( row    = 1
                            column = 1
                            text   = mv_prg_variants ).

    "Report title
    lo_header->create_label( row    = 1
                             column = 2
                             text   = align_text( iv_text  = mv_prg_title
                                                  iv_align = c_align_form_cell_centered ) ).

    "Date / time
    TRY.
        cl_abap_datfm=>conv_date_int_to_ext(
                                        EXPORTING
                                          im_datint = sy-datlo
                                        IMPORTING
                                          ex_datext = lv_date ).

      CATCH cx_abap_datfm_format_unknown.
        WRITE sy-datlo DD/MM/YYYY TO lv_date.
    ENDTRY.

    TRY.
        cl_abap_timefm=>conv_time_int_to_ext(
                                        EXPORTING
                                          time_int        = sy-timlo
                                          without_seconds = abap_true
                                        IMPORTING
                                          time_ext        = DATA(lv_time) ).
        lv_timec = lv_time.

      CATCH cx_parameter_invalid_range.
        WRITE sy-uzeit TO lv_timec.
    ENDTRY.

    lo_header->create_text(
                      row    = 1
                      column = 3
                      text   = align_text(
*                                     iv_text  = |{ TEXT-dat } { lv_date } / { lv_timec }|
                                     iv_text  = |{ lv_date } / { lv_timec }|
                                     iv_align = c_align_form_cell_right ) ).


    "* - * - * - * - * - * - *   L I N E  2   * - * - * - * - * - * - *
    "System / client
    lo_header->create_text(
                      row    = 2
                      column = 1
*                      text   = |{ TEXT-sys } { sy-sysid } / { sy-mandt }| ).
                      text   = |{ sy-sysid } / { sy-mandt }| ).

    "ALV list header
    lo_header->create_label( row    = 2
                             column = 2
                             text   = align_text(
                                            iv_text  = mv_list_title
                                            iv_align = c_align_form_cell_centered ) ).

    "User
    lo_header->create_text(
                      row    = 2
                      column = 3
                      text   = align_text(
*                                     iv_text  = |{ TEXT-usr } { sy-uname }|
                                     iv_text  = |{ sy-uname }|
                                     iv_align = c_align_form_cell_right ) ).


    "* - * - * - * - * - * - *   L I N E  3   * - * - * - * - * - * - *
    "Cell 1 in line 3 should be used for Org.unit and its description


    "Cell 2 in line 3 is for free usage


    "Page
    lo_header->create_text( row    = 3
                            column = 3
                            text   = align_text(
*                                            iv_text  = |{ TEXT-pag } { iv_page }|
                                            iv_text  = |{ iv_page }|
                                            iv_align = c_align_form_cell_right ) ).
  ENDMETHOD.                    "create_top_header


  METHOD create_top_selections.
    "-----------------------------------------------------------------*
    "   Output of selections into list header
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lt_isetab       TYPE slis_seldis_alv,
      lt_isetab_full  TYPE slis_seldis_alv,
      lv_sel_name     TYPE fieldname,
      lv_sel_text     TYPE text40,
      lv_text         TYPE string,
      lv_cnt_col_sels TYPE i,
      lv_cnt_row      TYPE i,
      lv_cnt_row_out  TYPE i.

    "Get selections inclusive field labels from program text pool
    CALL FUNCTION 'K_KKB_SELECTIONS_READ'
      EXPORTING
        v_program      = mv_prg_variants
        rs_sel_hide    = ms_hide_sels
      TABLES
        ct_isetab      = lt_isetab        "only used selections
        et_isetab_full = lt_isetab_full.  "all selections

    CASE mv_used_sels_only.
      WHEN abap_true.
        "Only selections with value requested

      WHEN abap_false.
        "All selections should be displayed
        CLEAR lt_isetab.
        APPEND LINES OF lt_isetab_full TO lt_isetab.
    ENDCASE.

    IF lt_isetab IS INITIAL.
      "No selections entered and  => leave
      RETURN.
    ENDIF.

    FREE lt_isetab_full.      "isn't needed anymore

    "Create grid layout with 3 columns to arrange selections in it
    DATA(lo_sel) = get_top_element( mo_top_options->element_id-selections ).

    "C a l c u l a t e   w i d t h   on base of line-size
    ms_col_width = calc_n_set_width_sel( lo_sel ).

    "Insert in row 1 + 2 an initial and a info line
    IF mv_header_format NE mo_top_options->format_type-header_only.
      lo_sel->add_row( ).
      lv_cnt_row_out = lv_cnt_row_out + 1.
    ENDIF.

    "Set info line
    lo_sel->create_label( row     = lv_cnt_row_out
                          column  = ( ms_col_width-no_cols DIV 2 )
                          text    = align_text( iv_text  = 'S e l e c t i o n   v a l u e s'(hse)
                                                iv_align = c_align_form_cell_centered )
                          rowspan = 5
                          colspan = ( ms_col_width-left - strlen( TEXT-hse ) ) / 2 ).

    "Detemine number of lines to know when to switch to next column
    DATA(lv_no_lines)  = lines( lt_isetab ) DIV ms_col_width-no_cols.  "e.g. 11 DIV 3 = 3.
    DATA(lv_no_isetab) = lines( lt_isetab ) MOD ms_col_width-no_cols.
    IF lv_no_isetab NE 0.   "Add 1 line if division has remainder
      lv_no_lines = lv_no_lines + 1.
    ENDIF.

    "Output of selections
    lv_cnt_row_out  = lv_cnt_row_out + 1.
    lo_sel->add_row( ).
    LOOP AT lt_isetab ASSIGNING FIELD-SYMBOL(<ls_isetab>).
      "Create a new grid for next selections
      IF lv_cnt_row EQ 0.
        lv_cnt_col_sels = lv_cnt_col_sels + 1.
        DATA(lo_grid_col) = lo_sel->create_grid( row    = lv_cnt_row_out
                                                 column = lv_cnt_col_sels ).
      ENDIF.

      "Set next row
      lv_cnt_row = lv_cnt_row + 1.

      "Start again with first column
      DATA(lv_cnt_col) = 1.
      "Is only for the first line available
      IF lv_sel_name NE <ls_isetab>-org_selname OR
         lv_cnt_row  EQ 1.
        IF <ls_isetab>-stext IS NOT INITIAL.
          lv_sel_text = <ls_isetab>-stext.
        ENDIF.
        "Column 1 - field label of selection field
        lo_grid_col->create_label( row    = lv_cnt_row
                                   column = lv_cnt_col
                                   text   = lv_sel_text ).
        lv_sel_name = <ls_isetab>-org_selname.
      ENDIF.

      lv_cnt_col = lv_cnt_col + 1.
      "Column 2 - In-/ exclude
      lo_grid_col->create_text( row    = lv_cnt_row
                                column = lv_cnt_col
                                text   = <ls_isetab>-sign0 ).
      lv_cnt_col = lv_cnt_col + 1.
      "Column 3 - comparison option
      lo_grid_col->create_text( row    = lv_cnt_row
                                column = lv_cnt_col
                                text   = <ls_isetab>-optio ).
      lv_cnt_col = lv_cnt_col + 1.
      "Column 4 - value from
      IF <ls_isetab>-sign0 IS INITIAL AND
         <ls_isetab>-optio IS INITIAL AND
         <ls_isetab>-valuf IS INITIAL AND
         <ls_isetab>-valut IS INITIAL.
        <ls_isetab>-valuf = '<empty>'(epy).
      ENDIF.

      lo_grid_col->create_text( row    = lv_cnt_row
                                column = lv_cnt_col
                                text   = <ls_isetab>-valuf ).
      lv_cnt_col = lv_cnt_col + 1.
      "Column 5 - value to
      lo_grid_col->create_text( row    = lv_cnt_row
                                column = lv_cnt_col
                                text   = <ls_isetab>-valut ).

      IF lv_cnt_row EQ lv_no_lines.
        lv_cnt_row = 0.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.                    "create_top_selections


  METHOD free.
    "-----------------------------------------------------------------*
    "   Release objects when closing application
    "-----------------------------------------------------------------*
    IF mo_container IS BOUND.
      mo_container->free( ).
    ENDIF.

    FREE: mo_alv_grid,
          mo_container,
          mo_cls_desc,
          mo_top,
          mo_salv,
          mr_table,
          mt_cols,
          ms_col_width,
          ms_hide_sels,
          mv_header_format,
          mv_list_title,
          mv_prg_title,
          mv_prg_variants,
          mv_top_activated,
          mv_top_sel_crea,
          mv_used_sels_only.
  ENDMETHOD.                    "free


  METHOD get_top_element.
    "-----------------------------------------------------------------*
    "   Create/determine layout object for TOP-OF-PAGE
    "-----------------------------------------------------------------*
    IF mo_top IS NOT BOUND.
      mo_top = NEW #( 1 ).      "Creates GRID with one column
    ENDIF.

    "Get row
    READ TABLE mo_top->t_rows INTO DATA(ls_row)
                              WITH KEY id = iv_element_id.
    IF sy-subrc NE 0.
      "No element found -> create a grid with 3 columns
      ro_top_element = mo_top->create_grid( row    = CONV #( iv_element_id )
                                            column = 1 ).
      ro_top_element->set_column_count( 3 ).

    ELSE.
      "Get column -> is always 1 at this point
      READ TABLE ls_row-r_row->t_elements INTO DATA(ls_element)
                                          WITH KEY id = 1.
      ro_top_element ?= ls_element-r_uie.
    ENDIF.
  ENDMETHOD.                    "get_top_element


  METHOD grab_underlying_alv_grid_ctrl.
    "-----------------------------------------------------------------*
    "   Get instance of ALV_GRID control for additional functionalities
    "   ATTENTION!! The determination of the ALV GRID instance is at the
    "   earliest possible directly after MO_SALV->DISPLAY was executed!!!
    "   Since this method will not be left until the user is leaving the
    "   program, an explicit function is necessary to be implemented and
    "   has to be used by the user, e. g. toggle function between display
    "   and change mode.
    "-----------------------------------------------------------------*
    "Leave if ALV GRID is already determined
    IF mo_alv_grid IS BOUND.
      RETURN.
    ENDIF.

    mo_alv_grid = NEW _access_to_alv_grid( )->get_control( mo_salv ).
  ENDMETHOD.                    "grab_underlying_alv_grid_ctrl


  METHOD on_added_function.
    "-----------------------------------------------------------------*
    "   Handle additional function
    "-----------------------------------------------------------------*
    "h a s   t o   b e   r e d e f i n e d
  ENDMETHOD.                    "on_added_function


  METHOD on_double_click.
    "-----------------------------------------------------------------*
    "   Handle double_click
    "-----------------------------------------------------------------*
    "h a s   t o   b e   r e d e f i n e d
  ENDMETHOD.                    "on_double_click


  METHOD on_link_click.
    "-----------------------------------------------------------------*
    "   Handle click at link/hotspot
    "-----------------------------------------------------------------*
    "h a s   t o   b e   r e d e f i n e d
  ENDMETHOD.                    "on_link_click


  METHOD on_top_of_page.
    "-----------------------------------------------------------------*
    "   Print output of the list header area (regard method docu)
    "-----------------------------------------------------------------*
    "To avoid the output of the selection on every page, delete the complete
    "page header, because removing only a row in form object is not possible.
    IF page EQ 2.
      CLEAR mo_top.
    ENDIF.

    "Creates a grid layout object with 3 rows and 3 columns. FREE to use
    "are the CELLS 1 and 2 in LINE 3.
    IF mv_header_format EQ mo_top_options->format_type-header_only          OR
       mv_header_format EQ mo_top_options->format_type-header_n_selections.
      create_top_header( page ).
    ENDIF.

    "Show also selection values below the header if requested
    IF   mv_top_sel_crea  EQ abap_true                  AND
       ( mv_header_format EQ mo_top_options->format_type-selections_only      OR
         mv_header_format EQ mo_top_options->format_type-header_n_selections ).
      create_top_selections( ).
      mv_top_sel_crea = abap_true.
    ENDIF.

    "Set content for Top-of-page, if this method is not redefined.
    IF line_exists( mo_cls_desc->methods[ name         = 'ON_TOP_OF_PAGE' ##no_text
                                          is_redefined = abap_true ] ).
      "* - * - * - * - * - * - *  - * - * - * - * - * - * - * - * - * - * - * - * - *
      " !!! EXECUTE THE FOLLOWING METHOD AT THE END, IF YOU REDEFINE THIS METHOD !!!
      r_top_of_page->set_content( mo_top ).
      "* - * - * - * - * - * - *  - * - * - * - * - * - * - * - * - * - * - * - * - *
    ENDIF.
  ENDMETHOD.                    "on_top_of_page


  METHOD prepare_alv.
    "-----------------------------------------------------------------*
    "   Do other ALV preparations
    "-----------------------------------------------------------------*
    "h a s   t o   b e   r e d e f i n e d

    " ! " ! " ! " ! " ! " ! " ! " ! " ! " ! " ! " ! " ! " ! " ! " ! " !
    "     Check out sample reports SALV_DEMO_TABLE_* to get more
    "     knowledge on how to manipulate SALV grids :)
    " ! " ! " ! " ! " ! " ! " ! " ! " ! " ! " ! " ! " ! " ! " ! " ! " !
  ENDMETHOD.                    "prepare_alv


  METHOD prepare_default.
    "-----------------------------------------------------------------*
    "   Do default/standard preparations
    "-----------------------------------------------------------------*
    "Set output options
    mo_salv->get_display_settings( )->set_striped_pattern( abap_true ).
    mo_salv->get_display_settings( )->set_list_header_size( cl_salv_display_settings=>c_header_size_medium ).
    mo_salv->get_display_settings( )->set_list_header( mv_list_title ).

    "Set restrictions for layout management
    mo_salv->get_layout( )->set_default( abap_true ).

    "Activate functions
    mo_salv->get_functions( )->set_all( ).
    mo_salv->get_functions( )->set_export_xml( ).
    "Deactivate some functions
    mo_salv->get_functions( )->set_export_wordprocessor( abap_false ).
    mo_salv->get_functions( )->set_view_lotus( abap_false ).

    "Column settings -> key column are fixed and all columns are optimized
    mo_salv->get_columns( )->set_optimize( ).
    mo_salv->get_columns( )->set_key_fixation( ).
    "Provide columns for preparations
    mt_cols = mo_salv->get_columns( )->get( ).
  ENDMETHOD.                    "prepare_default


  METHOD set_status.
    "-----------------------------------------------------------------*
    "   Set own PF status
    "-----------------------------------------------------------------*
    mo_salv->set_screen_status( pfstatus      = iv_pfstatus
                                report        = iv_prg_status
                                set_functions = iv_functions ).
  ENDMETHOD.                    "set_status

ENDCLASS.
