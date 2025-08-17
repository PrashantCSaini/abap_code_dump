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
       END OF ty_po_data.

TYPES: tt_po_data TYPE STANDARD TABLE OF ty_po_data.

TYPES: BEGIN OF ty_file_info,
         filename TYPE string,
         filepath TYPE string,
       END OF ty_file_info.

TYPES: tt_file_info TYPE STANDARD TABLE OF ty_file_info.

TYPES: tt_string TYPE STANDARD TABLE OF string.

" ALV Display Types
TYPES: BEGIN OF ty_alv_display,
         status_icon TYPE icon_d,      " Traffic light icon
         filename TYPE string,         " File name
         status TYPE string,           " Status text
         records_processed TYPE i,     " Number of records processed
         processing_time TYPE string,  " Processing timestamp
         message TYPE string,          " Detailed message
         message_type TYPE char1,      " S=Success, E=Error, W=Warning
       END OF ty_alv_display.

TYPES: tt_alv_display TYPE STANDARD TABLE OF ty_alv_display.

" Processing Log Types
TYPES: BEGIN OF ty_process_log,
         filename TYPE string,
         status TYPE char1,            " S=Success, E=Error
         records TYPE i,
         timestamp TYPE string,
         message TYPE string,
       END OF ty_process_log.

TYPES: tt_process_log TYPE STANDARD TABLE OF ty_process_log.

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

TYPES: tt_custom_table TYPE STANDARD TABLE OF ty_custom_table.

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
               IMPORTING 
                 iv_date TYPE dats
               RETURNING 
                 VALUE(rt_files) TYPE tt_file_info,
             read_csv_file
               IMPORTING 
                 iv_filepath TYPE string
               RETURNING 
                 VALUE(rt_data) TYPE tt_po_data
               RAISING   
                 cx_sy_file_open_mode
                 cx_sy_codepage_converter_init
                 cx_sy_conversion_codepage
                 cx_sy_file_access_error,
             save_to_custom_table
               IMPORTING 
                 it_data TYPE tt_po_data
               RAISING   
                 cx_sy_sql_error,
             move_file
               IMPORTING 
                 iv_source TYPE string
                 iv_target TYPE string
               RAISING   
                 cx_sy_file_access_error,
             display_results.

  PRIVATE SECTION.
    DATA: mv_processed_files TYPE i,
          mv_error_files     TYPE i,
          mt_messages        TYPE tt_string,
          mt_process_log     TYPE tt_process_log.

    METHODS: add_message
               IMPORTING 
                 iv_message TYPE string,
             add_process_log
               IMPORTING
                 iv_filename TYPE string
                 iv_status TYPE char1
                 iv_records TYPE i DEFAULT 0
                 iv_message TYPE string,
             validate_csv_data
               IMPORTING 
                 it_data TYPE tt_po_data
               RETURNING 
                 VALUE(rv_valid) TYPE abap_bool,
             format_filename_pattern
               IMPORTING 
                 iv_date TYPE dats
               RETURNING 
                 VALUE(rv_pattern) TYPE string,
             prepare_alv_data
               RETURNING
                 VALUE(rt_alv_data) TYPE tt_alv_display,
             display_alv_results
               IMPORTING
                 it_alv_data TYPE tt_alv_display.
ENDCLASS.

*----------------------------------------------------------------------*
* Class Implementation
*----------------------------------------------------------------------*
CLASS lcl_csv_processor IMPLEMENTATION.
  METHOD constructor.
    CLEAR: mv_processed_files, mv_error_files.
    REFRESH: mt_messages, mt_process_log.
  ENDMETHOD.

  METHOD process_files.
    DATA: lt_files TYPE tt_file_info,
          lt_data  TYPE tt_po_data,
          lv_source_path TYPE string,
          lv_archive_path TYPE string,
          lv_error_path TYPE string,
          ls_file TYPE ty_file_info,
          lo_exception TYPE REF TO cx_root.

    " Get all files for the specified date
    lt_files = get_files_for_date( iv_date ).

    IF lines( lt_files ) = 0.
      CONCATENATE 'No files found for date' iv_date INTO DATA(lv_message) SEPARATED BY space.
      add_message( lv_message ).
      RETURN.
    ENDIF.

    CONCATENATE 'Found' lines( lt_files ) 'file(s) for processing' INTO lv_message SEPARATED BY space.
    add_message( lv_message ).

    " Process each file
    LOOP AT lt_files INTO ls_file.
      TRY.
          " Read CSV file
          lt_data = read_csv_file( ls_file-filepath ).

          " Validate data
          IF validate_csv_data( lt_data ) = abap_true.
            " Save to custom table
            save_to_custom_table( lt_data ).

            " Move to archive folder
            CONCATENATE c_archive_path ls_file-filename INTO lv_archive_path.
            move_file( iv_source = ls_file-filepath
                      iv_target = lv_archive_path ).

            ADD 1 TO mv_processed_files.
            CONCATENATE 'Successfully processed:' ls_file-filename INTO lv_message SEPARATED BY space.
            add_message( lv_message ).
            
            " Add to process log
            add_process_log( iv_filename = ls_file-filename
                           iv_status = 'S'
                           iv_records = lines( lt_data )
                           iv_message = 'File processed successfully and moved to archive' ).

          ELSE.
            " Move to error folder due to validation issues
            CONCATENATE c_error_path ls_file-filename INTO lv_error_path.
            move_file( iv_source = ls_file-filepath
                      iv_target = lv_error_path ).

            ADD 1 TO mv_error_files.
            CONCATENATE 'Validation failed, moved to error:' ls_file-filename INTO lv_message SEPARATED BY space.
            add_message( lv_message ).
            
            " Add to process log
            add_process_log( iv_filename = ls_file-filename
                           iv_status = 'E'
                           iv_records = 0
                           iv_message = 'Data validation failed - moved to error folder' ).
          ENDIF.

        CATCH cx_sy_file_open_mode
              cx_sy_codepage_converter_init
              cx_sy_conversion_codepage
              cx_sy_file_access_error
              cx_sy_sql_error INTO lo_exception.

          " Move to error folder due to technical issues
          CONCATENATE c_error_path ls_file-filename INTO lv_error_path.
          TRY.
              move_file( iv_source = ls_file-filepath
                        iv_target = lv_error_path ).
            CATCH cx_sy_file_access_error.
              CONCATENATE 'Critical: Could not move error file' ls_file-filename INTO lv_message SEPARATED BY space.
              add_message( lv_message ).
          ENDTRY.

          ADD 1 TO mv_error_files.
          CONCATENATE 'Error processing' ls_file-filename ':' lo_exception->get_text( ) INTO lv_message SEPARATED BY space.
          add_message( lv_message ).
          
          " Add to process log
          add_process_log( iv_filename = ls_file-filename
                         iv_status = 'E'
                         iv_records = 0
                         iv_message = lo_exception->get_text( ) ).
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_files_for_date.
    DATA: lv_pattern TYPE string,
          lv_filename TYPE string,
          lv_filepath TYPE string,
          ls_file_info TYPE ty_file_info,
          lv_dd TYPE string,
          lv_mm TYPE string,
          lv_yyyy TYPE string,
          lt_file_list TYPE TABLE OF string,
          lv_file_entry TYPE string,
          lv_date_pattern TYPE string.

    " Create filename pattern for the date (PO_ddmmyyyy_*.csv)
    lv_yyyy = iv_date(4).
    lv_mm = iv_date+4(2).
    lv_dd = iv_date+6(2).
    CONCATENATE 'PO_' lv_dd lv_mm lv_yyyy INTO lv_date_pattern.

    TRY.
        " Method 1: Using EPS_GET_DIRECTORY_LISTING (if available)
        CALL FUNCTION 'EPS_GET_DIRECTORY_LISTING'
          EXPORTING
            dir_name               = c_input_path
            file_mask              = '*.csv'
          TABLES
            dir_list               = lt_file_list
          EXCEPTIONS
            invalid_eps_subdir     = 1
            sapgparam_failed       = 2
            build_directory_failed = 3
            no_authorization       = 4
            read_directory_failed  = 5
            too_many_read_errors   = 6
            empty_directory_list   = 7
            OTHERS                 = 8.

        IF sy-subrc = 0.
          " Filter files matching the date pattern
          LOOP AT lt_file_list INTO lv_file_entry.
            " Check if filename matches our pattern PO_ddmmyyyy_hhmmss.csv
            IF lv_file_entry CS lv_date_pattern AND
               lv_file_entry CP 'PO_*_*.csv'.
              
              ls_file_info-filename = lv_file_entry.
              CONCATENATE c_input_path lv_file_entry INTO ls_file_info-filepath.
              APPEND ls_file_info TO rt_files.
            ENDIF.
          ENDLOOP.
        ELSE.
          " Method 2: Alternative approach using OPEN DATASET with wildcard
          " This method uses a loop to check for files with different timestamps
          DATA: lv_hour TYPE string,
                lv_minute TYPE string,
                lv_second TYPE string,
                lv_timestamp TYPE string,
                lv_test_file TYPE string.

          " Try common time patterns (every hour from 00 to 23)
          DO 24 TIMES.
            lv_hour = sy-index - 1.
            IF strlen( lv_hour ) = 1.
              CONCATENATE '0' lv_hour INTO lv_hour.
            ENDIF.

            " Try different minute combinations
            DO 60 TIMES.
              lv_minute = sy-index - 1.
              IF strlen( lv_minute ) = 1.
                CONCATENATE '0' lv_minute INTO lv_minute.
              ENDIF.

              " Check only every 15 minutes to reduce overhead
              IF lv_minute = '00' OR lv_minute = '15' OR lv_minute = '30' OR lv_minute = '45'.
                lv_second = '00'.
                CONCATENATE lv_hour lv_minute lv_second INTO lv_timestamp.
                CONCATENATE 'PO_' lv_dd lv_mm lv_yyyy '_' lv_timestamp '.csv' INTO lv_filename.
                CONCATENATE c_input_path lv_filename INTO lv_test_file.

                " Test if file exists
                OPEN DATASET lv_test_file FOR INPUT IN TEXT MODE.
                IF sy-subrc = 0.
                  CLOSE DATASET lv_test_file.
                  ls_file_info-filename = lv_filename.
                  ls_file_info-filepath = lv_test_file.
                  APPEND ls_file_info TO rt_files.
                ENDIF.
              ENDIF.
            ENDDO.
          ENDDO.
        ENDIF.

      CATCH cx_root INTO DATA(lo_exception).
        " Method 3: Fallback - Manual file checking with common patterns
        DATA: lt_common_times TYPE TABLE OF string.

        " Common time patterns for testing
        APPEND '000000' TO lt_common_times.
        APPEND '060000' TO lt_common_times.
        APPEND '120000' TO lt_common_times.
        APPEND '180000' TO lt_common_times.
        APPEND '235959' TO lt_common_times.

        LOOP AT lt_common_times INTO lv_timestamp.
          CONCATENATE 'PO_' lv_dd lv_mm lv_yyyy '_' lv_timestamp '.csv' INTO lv_filename.
          CONCATENATE c_input_path lv_filename INTO lv_test_file.

          " Test if file exists
          OPEN DATASET lv_test_file FOR INPUT IN TEXT MODE.
          IF sy-subrc = 0.
            CLOSE DATASET lv_test_file.
            ls_file_info-filename = lv_filename.
            ls_file_info-filepath = lv_test_file.
            APPEND ls_file_info TO rt_files.
          ENDIF.
        ENDLOOP.

        " Log the exception for debugging
        CONCATENATE 'Directory listing failed:' lo_exception->get_text( ) INTO DATA(lv_error_msg) SEPARATED BY space.
        add_message( lv_error_msg ).
    ENDTRY.

  ENDMETHOD.

  METHOD read_csv_file.
    DATA: lv_line TYPE string,
          lt_fields TYPE tt_string,
          ls_data TYPE ty_po_data,
          lv_field TYPE string,
          lv_error_text TYPE string,
          lv_clean_line TYPE string.

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

      " Clean the line - remove unwanted characters at the end
      lv_clean_line = lv_line.
      
      " Remove common line ending characters and symbols
      REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN lv_clean_line WITH ''.
      REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN lv_clean_line WITH ''.
      REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr IN lv_clean_line WITH ''.
      REPLACE ALL OCCURRENCES OF '#' IN lv_clean_line WITH ''.
      REPLACE ALL OCCURRENCES OF '~' IN lv_clean_line WITH ''.
      
      " Remove trailing and leading spaces
      CONDENSE lv_clean_line.
      
      " Remove trailing special characters that might appear at end of line
      WHILE lv_clean_line CS '#' OR lv_clean_line CS '~' OR lv_clean_line CS '|'.
        REPLACE REGEX '[#~|]+$' IN lv_clean_line WITH ''.
      ENDWHILE.

      " Split CSV line
      SPLIT lv_clean_line AT ',' INTO TABLE lt_fields.

      " Validate field count
      IF lines( lt_fields ) <> 10.
        CLOSE DATASET iv_filepath.
        CONCATENATE 'Invalid CSV format: expected 10 fields, found' lines( lt_fields ) INTO lv_error_text SEPARATED BY space.
        RAISE EXCEPTION TYPE cx_sy_conversion_codepage
          EXPORTING
            text = lv_error_text.
      ENDIF.

      " Map fields to structure with additional cleaning
      CLEAR ls_data.
      READ TABLE lt_fields INDEX 1 INTO lv_field.
      ls_data-field1 = lv_field.
      READ TABLE lt_fields INDEX 2 INTO lv_field.
      ls_data-field2 = lv_field.
      READ TABLE lt_fields INDEX 3 INTO lv_field.
      ls_data-field3 = lv_field.
      READ TABLE lt_fields INDEX 4 INTO lv_field.
      ls_data-field4 = lv_field.
      READ TABLE lt_fields INDEX 5 INTO lv_field.
      ls_data-field5 = lv_field.
      READ TABLE lt_fields INDEX 6 INTO lv_field.
      ls_data-field6 = lv_field.
      READ TABLE lt_fields INDEX 7 INTO lv_field.
      ls_data-field7 = lv_field.
      READ TABLE lt_fields INDEX 8 INTO lv_field.
      ls_data-field8 = lv_field.
      READ TABLE lt_fields INDEX 9 INTO lv_field.
      ls_data-field9 = lv_field.
      READ TABLE lt_fields INDEX 10 INTO lv_field.
      ls_data-field10 = lv_field.

      " Clean each field individually
      PERFORM clean_field_data CHANGING ls_data-field1.
      PERFORM clean_field_data CHANGING ls_data-field2.
      PERFORM clean_field_data CHANGING ls_data-field3.
      PERFORM clean_field_data CHANGING ls_data-field4.
      PERFORM clean_field_data CHANGING ls_data-field5.
      PERFORM clean_field_data CHANGING ls_data-field6.
      PERFORM clean_field_data CHANGING ls_data-field7.
      PERFORM clean_field_data CHANGING ls_data-field8.
      PERFORM clean_field_data CHANGING ls_data-field9.
      PERFORM clean_field_data CHANGING ls_data-field10.

      APPEND ls_data TO rt_data.
    ENDDO.

    CLOSE DATASET iv_filepath.
  ENDMETHOD.

  METHOD save_to_custom_table.
    DATA: lt_custom_data TYPE tt_custom_table,
          ls_custom_data TYPE ty_custom_table,
          ls_csv_data TYPE ty_po_data,
          lv_message TYPE string.

    " Map CSV data to custom table structure
    LOOP AT it_data INTO ls_csv_data.
      CLEAR ls_custom_data.
      ls_custom_data-client = sy-mandt.
      ls_custom_data-field1 = ls_csv_data-field1.
      ls_custom_data-field2 = ls_csv_data-field2.
      ls_custom_data-field3 = ls_csv_data-field3.
      ls_custom_data-field4 = ls_csv_data-field4.
      ls_custom_data-field5 = ls_csv_data-field5.
      ls_custom_data-field6 = ls_csv_data-field6.
      ls_custom_data-field7 = ls_csv_data-field7.
      ls_custom_data-field8 = ls_csv_data-field8.
      ls_custom_data-field9 = ls_csv_data-field9.
      ls_custom_data-field10 = ls_csv_data-field10.
      ls_custom_data-created_on = sy-datum.
      ls_custom_data-created_at = sy-uzeit.
      APPEND ls_custom_data TO lt_custom_data.
    ENDLOOP.

    " Insert data into custom table
    " Replace 'ZCUSTOM_PO_TABLE' with your actual custom table name
    " INSERT ZCUSTOM_PO_TABLE FROM TABLE lt_custom_data.
    
    " For demonstration, we'll simulate the insert
    IF sy-subrc = 0.
      CONCATENATE 'Inserted' lines( lt_custom_data ) 'records into custom table' INTO lv_message SEPARATED BY space.
      add_message( lv_message ).
    ELSE.
      RAISE EXCEPTION TYPE cx_sy_sql_error.
    ENDIF.
  ENDMETHOD.

  METHOD move_file.
    DATA: lv_buffer TYPE xstring.

    " Open source file for reading
    OPEN DATASET iv_source FOR INPUT IN BINARY MODE.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE cx_sy_file_access_error
        EXPORTING
          filename = iv_source.
    ENDIF.

    " Open target file for writing
    OPEN DATASET iv_target FOR OUTPUT IN BINARY MODE.
    IF sy-subrc <> 0.
      CLOSE DATASET iv_source.
      RAISE EXCEPTION TYPE cx_sy_file_access_error
        EXPORTING
          filename = iv_target.
    ENDIF.

    " Copy file content
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
    DATA: ls_data TYPE ty_po_data,
          lv_message TYPE string.

    rv_valid = abap_true.

    " Add your validation logic here
    LOOP AT it_data INTO ls_data.
      " Example validations
      IF ls_data-field1 IS INITIAL OR
         ls_data-field2 IS INITIAL.
        rv_valid = abap_false.
        CONCATENATE 'Validation failed: Required fields are empty in line' sy-tabix INTO lv_message SEPARATED BY space.
        add_message( lv_message ).
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

    CONCATENATE 'PO_' lv_dd lv_mm lv_yyyy '_*.csv' INTO rv_pattern.
  ENDMETHOD.

  METHOD add_message.
    APPEND iv_message TO mt_messages.
  ENDMETHOD.

  METHOD add_process_log.
    DATA: ls_log TYPE ty_process_log,
          lv_time TYPE string.

    " Format current timestamp
    CONCATENATE sy-datum sy-uzeit INTO lv_time SEPARATED BY space.

    ls_log-filename = iv_filename.
    ls_log-status = iv_status.
    ls_log-records = iv_records.
    ls_log-timestamp = lv_time.
    ls_log-message = iv_message.
    
    APPEND ls_log TO mt_process_log.
  ENDMETHOD.

  METHOD prepare_alv_data.
    DATA: ls_alv TYPE ty_alv_display,
          ls_log TYPE ty_process_log.

    " Convert process log to ALV display format
    LOOP AT mt_process_log INTO ls_log.
      CLEAR ls_alv.
      
      " Set traffic light icon based on status
      CASE ls_log-status.
        WHEN 'S'.
          ls_alv-status_icon = '@08@'.  " Green light
          ls_alv-status = 'Success'.
          ls_alv-message_type = 'S'.
        WHEN 'E'.
          ls_alv-status_icon = '@0A@'.  " Red light
          ls_alv-status = 'Error'.
          ls_alv-message_type = 'E'.
        WHEN 'W'.
          ls_alv-status_icon = '@09@'.  " Yellow light
          ls_alv-status = 'Warning'.
          ls_alv-message_type = 'W'.
        WHEN OTHERS.
          ls_alv-status_icon = '@0B@'.  " Gray light
          ls_alv-status = 'Unknown'.
          ls_alv-message_type = 'I'.
      ENDCASE.
      
      ls_alv-filename = ls_log-filename.
      ls_alv-records_processed = ls_log-records.
      ls_alv-processing_time = ls_log-timestamp.
      ls_alv-message = ls_log-message.
      
      APPEND ls_alv TO rt_alv_data.
    ENDLOOP.
    
    " Add summary row
    IF lines( rt_alv_data ) > 0.
      CLEAR ls_alv.
      ls_alv-status_icon = '@0B@'.
      ls_alv-filename = 'SUMMARY'.
      ls_alv-status = 'Total'.
      CONCATENATE 'Success:' mv_processed_files ' / Error:' mv_error_files INTO ls_alv-message SEPARATED BY space.
      ls_alv-records_processed = mv_processed_files + mv_error_files.
      CONCATENATE sy-datum sy-uzeit INTO ls_alv-processing_time SEPARATED BY space.
      ls_alv-message_type = 'I'.
      APPEND ls_alv TO rt_alv_data.
    ENDIF.
  ENDMETHOD.

  METHOD display_alv_results.
    DATA: lo_alv TYPE REF TO cl_salv_table,
          lo_functions TYPE REF TO cl_salv_functions_list,
          lo_display TYPE REF TO cl_salv_display_settings,
          lo_columns TYPE REF TO cl_salv_columns_table,
          lo_column TYPE REF TO cl_salv_column_table,
          lx_msg TYPE REF TO cx_salv_msg.

    TRY.
        " Create ALV object
        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = lo_alv
          CHANGING
            t_table      = it_alv_data.

        " Enable all standard functions
        lo_functions = lo_alv->get_functions( ).
        lo_functions->set_all( abap_true ).

        " Set display settings
        lo_display = lo_alv->get_display_settings( ).
        lo_display->set_striped_pattern( abap_true ).
        lo_display->set_list_header( 'CSV File Processing Results - Traffic Light Status' ).

        " Get columns object
        lo_columns = lo_alv->get_columns( ).
        lo_columns->set_optimize( abap_true ).

        " Configure individual columns
        TRY.
            " Status Icon Column
            lo_column ?= lo_columns->get_column( 'STATUS_ICON' ).
            lo_column->set_short_text( 'Status' ).
            lo_column->set_medium_text( 'Status' ).
            lo_column->set_long_text( 'Processing Status' ).
            lo_column->set_output_length( 8 ).

            " Filename Column
            lo_column ?= lo_columns->get_column( 'FILENAME' ).
            lo_column->set_short_text( 'File Name' ).
            lo_column->set_medium_text( 'File Name' ).
            lo_column->set_long_text( 'File Name' ).
            lo_column->set_output_length( 30 ).

            " Status Text Column
            lo_column ?= lo_columns->get_column( 'STATUS' ).
            lo_column->set_short_text( 'Result' ).
            lo_column->set_medium_text( 'Result' ).
            lo_column->set_long_text( 'Processing Result' ).
            lo_column->set_output_length( 12 ).

            " Records Processed Column
            lo_column ?= lo_columns->get_column( 'RECORDS_PROCESSED' ).
            lo_column->set_short_text( 'Records' ).
            lo_column->set_medium_text( 'Records' ).
            lo_column->set_long_text( 'Records Processed' ).
            lo_column->set_output_length( 10 ).

            " Processing Time Column
            lo_column ?= lo_columns->get_column( 'PROCESSING_TIME' ).
            lo_column->set_short_text( 'Time' ).
            lo_column->set_medium_text( 'Proc. Time' ).
            lo_column->set_long_text( 'Processing Time' ).
            lo_column->set_output_length( 15 ).

            " Message Column
            lo_column ?= lo_columns->get_column( 'MESSAGE' ).
            lo_column->set_short_text( 'Message' ).
            lo_column->set_medium_text( 'Message' ).
            lo_column->set_long_text( 'Detailed Message' ).
            lo_column->set_output_length( 50 ).

            " Hide technical columns
            lo_column ?= lo_columns->get_column( 'MESSAGE_TYPE' ).
            lo_column->set_technical( abap_true ).

          CATCH cx_salv_not_found.
            " Column not found - continue
        ENDTRY.

        " Display the ALV
        lo_alv->display( ).

      CATCH cx_salv_msg INTO lx_msg.
        " Fallback to simple display
        WRITE: / 'Error displaying ALV:', lx_msg->get_text( ).
        
        " Display summary information
        WRITE: / 'Processing Summary:',
               / '=================='.
        CONCATENATE 'Total Files Processed:' mv_processed_files INTO DATA(lv_msg) SEPARATED BY space.
        WRITE: / lv_msg.
        CONCATENATE 'Files with Errors:' mv_error_files INTO lv_msg SEPARATED BY space.
        WRITE: / lv_msg.
    ENDTRY.
  ENDMETHOD.

  METHOD display_results.
    DATA: lt_alv_data TYPE tt_alv_display.

    " Prepare ALV data
    lt_alv_data = prepare_alv_data( ).

    " Display in ALV
    IF lines( lt_alv_data ) > 0.
      display_alv_results( lt_alv_data ).
    ELSE.
      " Fallback to simple display if no processing data
      WRITE: / 'CSV File Processing Results',
             / '================================'.
      
      CONCATENATE 'Processed Files:' mv_processed_files INTO DATA(lv_message) SEPARATED BY space.
      WRITE: / lv_message.
      
      CONCATENATE 'Error Files:' mv_error_files INTO lv_message SEPARATED BY space.
      WRITE: / lv_message.
      
      WRITE: / 'No files were processed.'.
    ENDIF.
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
* Subroutines
*----------------------------------------------------------------------*
FORM clean_field_data CHANGING cv_field TYPE string.
  " Remove quotes
  REPLACE ALL OCCURRENCES OF '"' IN cv_field WITH ''.
  REPLACE ALL OCCURRENCES OF '''' IN cv_field WITH ''.
  
  " Remove line ending characters
  REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN cv_field WITH ''.
  REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN cv_field WITH ''.
  REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr IN cv_field WITH ''.
  
  " Remove special characters that appear at end of fields
  REPLACE ALL OCCURRENCES OF '#' IN cv_field WITH ''.
  REPLACE ALL OCCURRENCES OF '~' IN cv_field WITH ''.
  REPLACE ALL OCCURRENCES OF '|' IN cv_field WITH ''.
  REPLACE ALL OCCURRENCES OF '^' IN cv_field WITH ''.
  
  " Remove any trailing/leading whitespace
  CONDENSE cv_field.
  
  " Remove any remaining non-printable characters using regex
  REPLACE REGEX '[^\x20-\x7E]' IN cv_field WITH '' IGNORING CASE.
  
  " Additional cleanup for common unwanted characters
  REPLACE REGEX '[\x00-\x1F\x7F-\x9F]' IN cv_field WITH '' IGNORING CASE.
ENDFORM.

*----------------------------------------------------------------------*
* Text Symbols
*----------------------------------------------------------------------*
* TEXT-001: Processing Parameters
