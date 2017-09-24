CLASS ycl_roman_numerals_converter DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES yif_roman_numerals_converter .

    ALIASES convert_number
      FOR yif_roman_numerals_converter~convert_number .
    ALIASES convert_numeral
      FOR yif_roman_numerals_converter~convert_numeral .

    TYPES:
      BEGIN OF ts_value_interval,
        min TYPE i,
        max TYPE i,
      END OF ts_value_interval .
    TYPES:
      BEGIN OF ts_roman_numerals_map,
        symbol TYPE char1,
        value  TYPE i,
      END OF ts_roman_numerals_map .
    TYPES:
      tt_roman_numerals_map TYPE STANDARD TABLE OF ts_roman_numerals_map WITH DEFAULT KEY .

    METHODS constructor .
    METHODS get_new_value_total
      IMPORTING
        !subtotal    TYPE i
        !symbol      TYPE char1
        !operand     TYPE char1
      RETURNING
        VALUE(value) TYPE i .
    METHODS get_symbol_stack
      IMPORTING
        !numeral       TYPE string
      RETURNING
        VALUE(symbols) TYPE stringtab .
    METHODS get_numeral_value
      IMPORTING
        !symbols     TYPE stringtab
      RETURNING
        VALUE(value) TYPE i .
    METHODS get_number_of_digits
      IMPORTING
        !number                 TYPE i
      RETURNING
        VALUE(number_of_digits) TYPE i .
    METHODS get_number_at_position
      IMPORTING
        !position     TYPE i
        !number       TYPE i
      RETURNING
        VALUE(result) TYPE i .
    METHODS convert_number_to_string
      IMPORTING
        !number       TYPE i
      RETURNING
        VALUE(string) TYPE string .
    METHODS get_symbol_by_value
      IMPORTING
        !number       TYPE i
      RETURNING
        VALUE(symbol) TYPE string .
    METHODS get_number_symbol
      IMPORTING
        !number       TYPE i
      RETURNING
        VALUE(symbol) TYPE string .
    METHODS calculate_number_symbol
      IMPORTING
        !number       TYPE i
      RETURNING
        VALUE(result) TYPE string .
    METHODS get_value_interval
      IMPORTING
        !i_number       TYPE i
      RETURNING
        VALUE(interval) TYPE ts_value_interval .
    METHODS map_number_to_numeral
      IMPORTING
        !number       TYPE i
        !interval     TYPE ts_value_interval
      RETURNING
        VALUE(result) TYPE string .
    METHODS get_sign_multiplicator
      IMPORTING
        !number       TYPE i
      RETURNING
        VALUE(result) TYPE i .
    METHODS get_symbol_value IMPORTING symbol TYPE char1 RETURNING VALUE(value) TYPE i.
    METHODS get_operand
        IMPORTING right_symbol TYPE char1 left_symbol TYPE char1 RETURNING VALUE(operand) TYPE char1.
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mt_roman_numerals_value_map TYPE tt_roman_numerals_map .

    METHODS get_flat_number
      IMPORTING
        !prefix       TYPE string
        !no_of_zeros  TYPE i
      RETURNING
        VALUE(result) TYPE i .
ENDCLASS.



CLASS YCL_ROMAN_NUMERALS_CONVERTER IMPLEMENTATION.


  METHOD calculate_number_symbol.
    IF number >= 1000.
      DO number / 1000 TIMES.
        result = result && get_number_symbol( 1000 ).
      ENDDO.
    ELSE.
      DATA(ls_interval) = get_value_interval( number ).
      result = map_number_to_numeral( number = number interval = ls_interval ).
    ENDIF.

  ENDMETHOD.


  METHOD constructor.
    APPEND VALUE #( symbol = 'I' value = 1 )    TO mt_roman_numerals_value_map.
    APPEND VALUE #( symbol = 'V' value = 5 )    TO mt_roman_numerals_value_map.
    APPEND VALUE #( symbol = 'X' value = 10 )   TO mt_roman_numerals_value_map.
    APPEND VALUE #( symbol = 'L' value = 50 )   TO mt_roman_numerals_value_map.
    APPEND VALUE #( symbol = 'C' value = 100 )  TO mt_roman_numerals_value_map.
    APPEND VALUE #( symbol = 'D' value = 500 )  TO mt_roman_numerals_value_map.
    APPEND VALUE #( symbol = 'M' value = 1000 ) TO mt_roman_numerals_value_map.
  ENDMETHOD.


  METHOD convert_number_to_string.

    DATA lv_number TYPE string.
    string = number.
    CONDENSE string.

  ENDMETHOD.


  METHOD get_flat_number.
    DATA lv_numc TYPE string.
    lv_numc = prefix.
    DO no_of_zeros TIMES.
      lv_numc = lv_numc && '0'.
    ENDDO.
    result = lv_numc.
  ENDMETHOD.


  METHOD get_new_value_total.
    value = SWITCH #( operand WHEN '+' THEN subtotal + get_symbol_value( symbol ) WHEN '-' THEN  subtotal - get_symbol_value( symbol ) ).
  ENDMETHOD.


  METHOD get_number_at_position.
    DATA lv_number TYPE string.
    lv_number = convert_number_to_string( number ).
    result = lv_number+0(1).

    DO get_number_of_digits( number ) - 1 TIMES.
      result = result && '0'.
    ENDDO.
  ENDMETHOD.


  METHOD get_number_of_digits.
    DATA lv_number TYPE string.


    lv_number = convert_number_to_string( number ).
    number_of_digits = strlen( lv_number ).
  ENDMETHOD.


  METHOD get_number_symbol.

    symbol = mt_roman_numerals_value_map[ value = number ]-symbol.

  ENDMETHOD.


  METHOD get_numeral_value.
    DATA i TYPE syindex VALUE 2.

    value = get_symbol_value( CONV #( symbols[ 1 ] ) ).
    IF lines( symbols ) = 1.
      RETURN.
    ENDIF.

    WHILE line_exists( symbols[ i ] ).
      value = get_new_value_total(
          operand   = get_operand( right_symbol = CONV #( symbols[ i - 1 ] ) left_symbol = CONV #( symbols[ i ] ) )
          subtotal  = value
          symbol    =  CONV #( symbols[ i ] ) ).
      ADD 1 TO i.
    ENDWHILE.

  ENDMETHOD.


    METHOD get_operand.
    operand = COND #( WHEN get_symbol_value( left_symbol ) < get_symbol_value( right_symbol ) THEN '-' ELSE '+' ).
  ENDMETHOD.


  METHOD get_sign_multiplicator.
    result = COND #( WHEN number < 4 THEN 1 ELSE -1 ).
  ENDMETHOD.


  METHOD get_symbol_by_value.
    IF line_exists( mt_roman_numerals_value_map[ value = number ] ).
      symbol = get_number_symbol( number ).
      RETURN.
    ENDIF.

    IF number > 1000.
      DO number / 1000 TIMES.
        symbol = symbol && get_number_symbol( 1000 ).
      ENDDO.
    ELSE.
      symbol = calculate_number_symbol( number ).
    ENDIF.
  ENDMETHOD.


  METHOD get_symbol_stack.
    DATA(length) = strlen( numeral ).

    symbols = COND #(
        WHEN length = 1
        THEN VALUE #( ( numeral ) )
        ELSE VALUE #( FOR i = length - 1 THEN i - 1 WHILE i >= 0 ( numeral+i(1) ) )
            ).
  ENDMETHOD.


  METHOD get_symbol_value.
    value = mt_roman_numerals_value_map[ symbol = symbol ]-value.
  ENDMETHOD.


  METHOD get_value_interval.
    LOOP AT mt_roman_numerals_value_map INTO DATA(ls_max) WHERE value > i_number.
      DATA(lv_max_index) = sy-tabix.
      interval-max = ls_max-value.
      EXIT.
    ENDLOOP.

    READ TABLE mt_roman_numerals_value_map INTO DATA(ls_min) INDEX lv_max_index - 1.
    IF sy-subrc = 0.
      interval-min = ls_min-value.
    ENDIF.
  ENDMETHOD.


  METHOD map_number_to_numeral.
    DATA lv_numc TYPE string.
    DATA lv_factor TYPE i.
    DATA(lv_rest) = number MOD interval-min.

    IF lv_rest > 0.
      DATA(lv_rest_found) = abap_true.
      DATA(lv_no_of_zeros) = get_number_of_digits( lv_rest ) - 1.
      lv_numc = convert_number_to_string( lv_rest ).
      lv_numc = lv_numc+0(1).
      lv_rest = lv_numc.

      lv_numc = '1'.
      DO lv_no_of_zeros TIMES.
        lv_numc = lv_numc && '0'.
      ENDDO.
      lv_factor = lv_numc.

    ELSE.
      lv_rest = number / interval-min.
      lv_factor = interval-min.
    ENDIF.

    DATA(sign_multiplicator) = get_sign_multiplicator( lv_rest ) .

    IF sign_multiplicator < 0.
      result = get_number_symbol( lv_factor ) && get_number_symbol( interval-max ).
    ELSE.
      IF lv_rest_found = abap_true.
        result = get_number_symbol( interval-min ).
      ENDIF.
      DO lv_rest TIMES.
        result = result && get_number_symbol( lv_factor ).
      ENDDO.
    ENDIF.
  ENDMETHOD.


  METHOD yif_roman_numerals_converter~convert_number.
    DATA lv_numc  TYPE string.
    DATA i TYPE i VALUE 1.
    DATA lt_numbers TYPE int_tab1.

    lv_numc = convert_number_to_string( number ).
    DATA(lv_index) = strlen( lv_numc ).
    WHILE i < lv_index.
      DATA(no_of_zeros) = get_number_of_digits( CONV #( lv_numc ) ) - 1.
      DATA(prefix) = lv_numc+0(1).
      APPEND get_flat_number( prefix = prefix no_of_zeros = no_of_zeros ) TO lt_numbers.
      DATA(offset) = lv_index - i.
      lv_numc = lv_numc+1(offset).
      ADD 1 TO i.
    ENDWHILE.
    APPEND lv_numc TO lt_numbers.

    LOOP AT lt_numbers INTO DATA(lv_number).
      result = result && calculate_number_symbol( lv_number ).
    ENDLOOP.
  ENDMETHOD.


  METHOD yif_roman_numerals_converter~convert_numeral.
    value = get_numeral_value( get_symbol_stack( numeral ) ).
  ENDMETHOD.
ENDCLASS.
