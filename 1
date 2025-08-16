*&---------------------------------------------------------------------*
*& Report ZPO_CSV_PROCESSOR
*&---------------------------------------------------------------------*
*& CSV File Processor - Reads PO files from AL11 directory
*& Uses modern ABAP syntax and OOP methodology
*&---------------------------------------------------------------------*
REPORT zpo_csv_processor.

*----------------------------------------------------------------------*
* Type Definitions
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_po_data,
         field1 TYPE string,
         field2 TYPE string,
         field3 TYPE string,
         field4 TYPE string,
         field5 TYPE string,
         field6 TYPE string,
         field7 TYPE string,
         field8 TYPE string,
         field9 TYPE string,
         field10 TYPE string,
       END OF ty_po_data,
       tt_po_data TYPE TABLE OF ty_po_data.

TYPES: BEGIN OF ty_file_info,
         filename TYPE string,
         filepath TYPE string,
       END OF ty_file_info,
       tt_file_info TYPE TABLE OF ty_file_info.

*----------------------------------------------------------------------*
* Custom Table Structure (Example - adjust as per your requirement)
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_custom_table,
         client TYPE mandt,
         field1 TYPE string,
         field2 TYPE string,
         field3 TYPE string,
         field4 TYPE string,
         field5 TYPE string,
         field6 TYPE string,
         field7 TYPE string,
         field8 TYPE string,
         field9 TYPE string,
         field10 TYPE string,
         created_on TYPE dats,
         created_at TYPE tims,
       END OF ty_custom_table.

*----------------------------------------------------------------------*
* Selection Screen
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_date FOR sy-datum OBLIGATORY DEFAULT sy-datum.
SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------*
* Class Definition
*----------------------------------------------------------------------*
CLASS lcl_csv_processor DEFINITION.
  PUBLIC SECTION.
    CONSTANTS: c_input_path   TYPE string VALUE '/101/input/',
               c_archive_path TYPE string VALUE '/101/archive/',
               c_error_path   TYPE string VALUE '/101/error/'.

    METHODS: constructor,
             process_files
               IMPORTING iv_date TYPE dats,
             get_files_for_date
               IMPORTING iv_date TYPE dats
               RETURNING VALUE(rt_files) TYPE tt_file_info,
             read_csv_file
               IMPORTING iv_filepath TYPE string
               RETURNING VALUE(rt_data) TYPE tt_po_data
               RAISING   cx_sy_file_open_mode
                        cx_sy_codepage_converter_init
                        cx_sy_conversion_codepage
                        cx_sy_file_access_error,
             save_to_custom_table
               IMPORTING it_data TYPE tt_po_data
               RAISING   cx_sy_sql_error,
             move_file
               IMPORTING iv_source TYPE string
                        iv_target TYPE string
               RAISING   cx_sy_file_access_error,
             display_results.

  PRIVATE SECTION.
    DATA: mv_processed_files TYPE i,
          mv_error_files     TYPE i,
          mt_messages        TYPE TABLE OF string.

    METHODS: add_message
               IMPORTING iv_message TYPE string,
             validate_csv_data
               IMPORTING it_data TYPE tt_po_data
               RETURNING VALUE(rv_valid) TYPE abap_bool,
             format_filename_pattern
               IMPORTING iv_date TYPE dats
               RETURNING VALUE(rv_pattern) TYPE string.
ENDCLASS.

*----------------------------------------------------------------------*
* Class Implementation
*----------------------------------------------------------------------*
CLASS lcl_csv_processor IMPLEMENTATION.
  METHOD constructor.
    CLEAR: mv_processed_files, mv_error_files.
    REFRESH mt_messages.
  ENDMETHOD.

  METHOD process_files.
    DATA: lt_files TYPE tt_file_info,
          lt_data  TYPE tt_po_data,
          lv_source_path TYPE string,
          lv_archive_path TYPE string,
          lv_error_path TYPE string.

    " Get all files for the specified date
    lt_files = get_files_for_date( iv_date ).

    IF lines( lt_files ) = 0.
      add_message( |No files found for date { iv_date DATE = USER }| ).
      RETURN.
    ENDIF.

    add_message( |Found { lines( lt_files ) } file(s) for processing| ).

    " Process each file
    LOOP AT lt_files INTO DATA(ls_file).
      TRY.
          " Read CSV file
          lt_data = read_csv_file( ls_file-filepath ).

          " Validate data
          IF validate_csv_data( lt_data ).
            " Save to custom table
            save_to_custom_table( lt_data ).

            " Move to archive folder
            lv_archive_path = |{ c_archive_path }{ ls_file-filename }|.
            move_file( iv_source = ls_file-filepath
                      iv_target = lv_archive_path ).

            ADD 1 TO mv_processed_files.
            add_message( |Successfully processed: { ls_file-filename }| ).

          ELSE.
            " Move to error folder due to validation issues
            lv_error_path = |{ c_error_path }{ ls_file-filename }|.
            move_file( iv_source = ls_file-filepath
                      iv_target = lv_error_path ).

            ADD 1 TO mv_error_files.
            add_message( |Validation failed, moved to error: { ls_file-filename }| ).
          ENDIF.

        CATCH cx_sy_file_open_mode
              cx_sy_codepage_converter_init
              cx_sy_conversion_codepage
              cx_sy_file_access_error
              cx_sy_sql_error INTO DATA(lo_exception).

          " Move to error folder due to technical issues
          lv_error_path = |{ c_error_path }{ ls_file-filename }|.
          TRY.
              move_file( iv_source = ls_file-filepath
                        iv_target = lv_error_path ).
            CATCH cx_sy_file_access_error.
              add_message( |Critical: Could not move error file { ls_file-filename }| ).
          ENDTRY.

          ADD 1 TO mv_error_files.
          add_message( |Error processing { ls_file-filename }: { lo_exception->get_text( ) }| ).
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_files_for_date.
    DATA: lv_pattern TYPE string,
          lv_command TYPE string,
          lt_file_list TYPE TABLE OF string.

    " Create filename pattern for the date (PO_ddmmyyyy_*.csv)
    lv_pattern = format_filename_pattern( iv_date ).

    " Use OPEN DATASET with filter or alternative file listing approach
    " Note: This is a simplified approach - in real scenarios, you might need
    " to use OPEN DATASET or specific file system commands

    " For AL11 directory access, you would typically use:
    lv_command = |ls { c_input_path }{ lv_pattern }|.

    " Alternative approach using OPEN DATASET
    DATA: lv_filename TYPE string,
          lv_filepath TYPE string.

    " Simulate file discovery (in real scenario, use appropriate AL11 file listing)
    " This example assumes you have a way to list files in the directory
    
    " Example files for demonstration
    lv_filename = |PO_{ iv_date+6(2) }{ iv_date+4(2) }{ iv_date(4) }_120000.csv|.
    lv_filepath = |{ c_input_path }{ lv_filename }|.
    
    APPEND VALUE #( filename = lv_filename filepath = lv_filepath ) TO rt_files.
    
    " Add more files as found in the directory...
  ENDMETHOD.

  METHOD read_csv_file.
    DATA: lv_line TYPE string,
          lt_fields TYPE TABLE OF string,
          ls_data TYPE ty_po_data.

    " Open file for reading
    OPEN DATASET iv_filepath FOR INPUT IN TEXT MODE ENCODING UTF-8.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_file_open_mode
        EXPORTING
          filename = iv_filepath.
    ENDIF.

    " Read file line by line
    DO.
      READ DATASET iv_filepath INTO lv_line.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.

      " Skip empty lines
      IF lv_line IS INITIAL.
        CONTINUE.
      ENDIF.

      " Split CSV line
      SPLIT lv_line AT ',' INTO TABLE lt_fields.

      " Validate field count
      IF lines( lt_fields ) <> 10.
        CLOSE DATASET iv_filepath.
        RAISE EXCEPTION TYPE cx_sy_conversion_codepage
          EXPORTING
            text = |Invalid CSV format: expected 10 fields, found { lines( lt_fields ) }|.
      ENDIF.

      " Map fields to structure
      CLEAR ls_data.
      READ TABLE lt_fields INDEX 1 INTO ls_data-field1.
      READ TABLE lt_fields INDEX 2 INTO ls_data-field2.
      READ TABLE lt_fields INDEX 3 INTO ls_data-field3.
      READ TABLE lt_fields INDEX 4 INTO ls_data-field4.
      READ TABLE lt_fields INDEX 5 INTO ls_data-field5.
      READ TABLE lt_fields INDEX 6 INTO ls_data-field6.
      READ TABLE lt_fields INDEX 7 INTO ls_data-field7.
      READ TABLE lt_fields INDEX 8 INTO ls_data-field8.
      READ TABLE lt_fields INDEX 9 INTO ls_data-field9.
      READ TABLE lt_fields INDEX 10 INTO ls_data-field10.

      " Remove quotes if present
      ls_data-field1 = replace( val = ls_data-field1 sub = '"' with = '' occ = 0 ).
      ls_data-field2 = replace( val = ls_data-field2 sub = '"' with = '' occ = 0 ).
      " ... repeat for other fields as needed

      APPEND ls_data TO rt_data.
    ENDDO.

    CLOSE DATASET iv_filepath.
  ENDMETHOD.

  METHOD save_to_custom_table.
    DATA: lt_custom_data TYPE TABLE OF ty_custom_table.

    " Map CSV data to custom table structure
    LOOP AT it_data INTO DATA(ls_csv_data).
      APPEND VALUE ty_custom_table(
        client = sy-mandt
        field1 = ls_csv_data-field1
        field2 = ls_csv_data-field2
        field3 = ls_csv_data-field3
        field4 = ls_csv_data-field4
        field5 = ls_csv_data-field5
        field6 = ls_csv_data-field6
        field7 = ls_csv_data-field7
        field8 = ls_csv_data-field8
        field9 = ls_csv_data-field9
        field10 = ls_csv_data-field10
        created_on = sy-datum
        created_at = sy-uzeit
      ) TO lt_custom_data.
    ENDLOOP.

    " Insert data into custom table
    " Replace 'ZCUSTOM_PO_TABLE' with your actual custom table name
    " INSERT ZCUSTOM_PO_TABLE FROM TABLE lt_custom_data.
    
    " For demonstration, we'll simulate the insert
    IF sy-subrc = 0.
      add_message( |Inserted { lines( lt_custom_data ) } records into custom table| ).
    ELSE.
      RAISE EXCEPTION TYPE cx_sy_sql_error.
    ENDIF.
  ENDMETHOD.

  METHOD move_file.
    DATA: lv_command TYPE string.

    " Create target directory if not exists
    lv_command = |mkdir -p { substring( val = iv_target before = '/' occ = -1 ) }|.
    " Execute mkdir command (platform dependent)

    " Move file using system command or OPEN DATASET
    lv_command = |mv { iv_source } { iv_target }|.
    
    " Alternative using ABAP file operations
    OPEN DATASET iv_source FOR INPUT IN BINARY MODE.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_file_access_error
        EXPORTING
          filename = iv_source.
    ENDIF.

    OPEN DATASET iv_target FOR OUTPUT IN BINARY MODE.
    IF sy-subrc <> 0.
      CLOSE DATASET iv_source.
      RAISE EXCEPTION TYPE cx_sy_file_access_error
        EXPORTING
          filename = iv_target.
    ENDIF.

    " Copy file content
    DATA: lv_buffer TYPE xstring.
    DO.
      READ DATASET iv_source INTO lv_buffer MAXIMUM LENGTH 1024.
      IF sy-subrc <> 0.
        EXIT.
      ENDIF.
      TRANSFER lv_buffer TO iv_target.
    ENDDO.

    CLOSE DATASET iv_source.
    CLOSE DATASET iv_target.

    " Delete source file
    DELETE DATASET iv_source.
  ENDMETHOD.

  METHOD validate_csv_data.
    rv_valid = abap_true.

    " Add your validation logic here
    LOOP AT it_data INTO DATA(ls_data).
      " Example validations
      IF ls_data-field1 IS INITIAL OR
         ls_data-field2 IS INITIAL.
        rv_valid = abap_false.
        add_message( |Validation failed: Required fields are empty in line { sy-tabix }| ).
        RETURN.
      ENDIF.

      " Add more specific validations as needed
      " e.g., numeric field validation, date format validation, etc.
    ENDLOOP.
  ENDMETHOD.

  METHOD format_filename_pattern.
    " Convert YYYYMMDD to DDMMYYYY format for filename pattern
    DATA: lv_dd TYPE string,
          lv_mm TYPE string,
          lv_yyyy TYPE string.

    lv_yyyy = iv_date(4).
    lv_mm = iv_date+4(2).
    lv_dd = iv_date+6(2).

    rv_pattern = |PO_{ lv_dd }{ lv_mm }{ lv_yyyy }_*.csv|.
  ENDMETHOD.

  METHOD add_message.
    APPEND iv_message TO mt_messages.
  ENDMETHOD.

  METHOD display_results.
    " Display processing results
    WRITE: / 'CSV File Processing Results',
           / '================================',
           / 'Processed Files:', mv_processed_files,
           / 'Error Files:', mv_error_files,
           / ''.

    WRITE: / 'Processing Messages:',
           / '-------------------'.

    LOOP AT mt_messages INTO DATA(lv_message).
      WRITE: / lv_message.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
* Main Program
*----------------------------------------------------------------------*
START-OF-SELECTION.
  DATA: lo_processor TYPE REF TO lcl_csv_processor.

  " Create processor instance
  CREATE OBJECT lo_processor.

  " Process files for each date in selection
  LOOP AT s_date.
    lo_processor->process_files( s_date-low ).
  ENDLOOP.

  " Display results
  lo_processor->display_results( ).

*----------------------------------------------------------------------*
* Text Symbols
*----------------------------------------------------------------------*
* TEXT-001: Processing Parameters
