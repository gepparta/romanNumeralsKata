CLASS lc_roman_numerals_convert_test DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CLASS-DATA mo_ref TYPE REF TO ycl_roman_numerals_converter.
    CLASS-METHODS class_setup.
    METHODS: test_get_operand_plus FOR TESTING RAISING cx_static_check.
    METHODS: test_get_operand_minus FOR TESTING RAISING cx_static_check.
    METHODS: test_get_new_value_total_add FOR TESTING RAISING cx_static_check.
    METHODS: test_get_new_value_total_sub FOR TESTING RAISING cx_static_check.
    METHODS: test_get_symbol_stack_n FOR TESTING RAISING cx_static_check.
    METHODS: test_get_symbol_stack_1 FOR TESTING RAISING cx_static_check.
    METHODS: test_get_numeral_value FOR TESTING RAISING cx_static_check.
    METHODS: test_convert_numeral FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS lc_roman_numerals_convert_test IMPLEMENTATION.

  METHOD test_get_operand_plus.
    cl_aunit_assert=>assert_equals(
        exp = '+'
        act =  mo_ref->get_operand( left_symbol = 'X' right_symbol = 'I' )
        ).
  ENDMETHOD.

  METHOD class_setup.
    mo_ref = NEW #( ).
  ENDMETHOD.

  METHOD test_get_operand_minus.
    cl_aunit_assert=>assert_equals(
        exp = '-'
        act =  mo_ref->get_operand( left_symbol = 'I' right_symbol = 'X' )
        ).
  ENDMETHOD.



  METHOD test_get_new_value_total_add.
    cl_aunit_assert=>assert_equals(
        exp = 15
        act = mo_ref->get_new_value_total( subtotal = 10 symbol = 'V' operand  = '+' )
          ).
  ENDMETHOD.

  METHOD test_get_new_value_total_sub.
    cl_aunit_assert=>assert_equals(
        exp = 5
        act = mo_ref->get_new_value_total( subtotal = 10 symbol = 'V' operand  = '-' )
          ).
  ENDMETHOD.

  METHOD test_get_symbol_stack_n.
    DATA lt_symbols TYPE stringtab.

    lt_symbols = mo_ref->get_symbol_stack( numeral = 'MDXI').
    cl_aunit_assert=>assert_equals( exp = 'I' act = lt_symbols[ 1 ] quit = cl_aunit_assert=>no ).
    cl_aunit_assert=>assert_equals( exp = 'X' act = lt_symbols[ 2 ] quit = cl_aunit_assert=>no ).
    cl_aunit_assert=>assert_equals( exp = 'D' act = lt_symbols[ 3 ] quit = cl_aunit_assert=>no ).
    cl_aunit_assert=>assert_equals( exp = 'M' act = lt_symbols[ 4 ] quit = cl_aunit_assert=>no ).
  ENDMETHOD.

  METHOD test_get_symbol_stack_1.
    DATA lt_symbols TYPE stringtab.

    lt_symbols = mo_ref->get_symbol_stack( numeral = 'M').
    cl_aunit_assert=>assert_equals( exp = 'M' act = lt_symbols[ 1 ] quit = cl_aunit_assert=>no ).
  ENDMETHOD.

  METHOD test_get_numeral_value.
    DATA lt_symbols TYPE stringtab.

    APPEND 'X' TO lt_symbols.
    APPEND 'I' TO lt_symbols.
    APPEND 'D' TO lt_symbols.
    APPEND 'M' TO lt_symbols.

    cl_aunit_assert=>assert_equals( exp = 1509 act = mo_ref->get_numeral_value( symbols = lt_symbols ) ).
  ENDMETHOD.

  METHOD test_convert_numeral.
    cl_aunit_assert=>assert_equals( exp = 1981 act = mo_ref->convert_numeral( 'MCMLXXXI' )  quit = cl_aunit_assert=>no  ).
    cl_aunit_assert=>assert_equals( exp = 1440 act = mo_ref->convert_numeral( 'MCDXL' )     quit = cl_aunit_assert=>no  ).
    cl_aunit_assert=>assert_equals( exp = 3576 act = mo_ref->convert_numeral( 'MMMDLXXVI' ) quit = cl_aunit_assert=>no  ).
  ENDMETHOD.

ENDCLASS.

CLASS lcl_number_to_numeral_test DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CLASS-DATA mo_ref TYPE REF TO ycl_roman_numerals_converter.
    CLASS-METHODS class_setup.
    METHODS: test_get_number_of_digits FOR TESTING RAISING cx_static_check.
    METHODS: test_convert_to_string FOR TESTING RAISING cx_static_check.
    METHODS: test_get_number_at_position FOR TESTING RAISING cx_static_check.
    METHODS: test_get_symbol_3000 FOR TESTING RAISING cx_static_check.
    METHODS: test_calculate_number FOR TESTING RAISING cx_static_check.
    METHODS: test_get_interval FOR TESTING RAISING cx_static_check.
    METHODS: test_get_fraction FOR TESTING RAISING cx_static_check.
    METHODS: test_get_sign_multiplicator FOR TESTING RAISING cx_static_check.
    METHODS: test_conver_number FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS lcl_number_to_numeral_test IMPLEMENTATION.

  METHOD class_setup.
    mo_ref = NEW #( ).
  ENDMETHOD.

  METHOD test_get_number_of_digits.
    cl_aunit_assert=>assert_equals( exp = 4 act = mo_ref->get_number_of_digits( 1981 ) ).
  ENDMETHOD.


  METHOD test_get_number_at_position.
    cl_aunit_assert=>assert_equals( exp = 1000 act = mo_ref->get_number_at_position( position = 1 number = 1981 ) ).
  ENDMETHOD.

  METHOD test_convert_to_string.
    cl_aunit_assert=>assert_equals( exp = '1981' act = mo_ref->convert_number_to_string( 1981 ) ).
  ENDMETHOD.

  METHOD test_get_symbol_3000.
    cl_aunit_assert=>assert_equals( exp = 'MMM' act = mo_ref->get_symbol_by_value( 3000 ) ).
  ENDMETHOD.

  METHOD test_calculate_number.
    cl_aunit_assert=>assert_equals( exp = 'CD'   act = mo_ref->calculate_number_symbol( 400 ) quit = cl_aunit_assert=>no ).
    cl_aunit_assert=>assert_equals( exp = 'CCC'  act = mo_ref->calculate_number_symbol( 300 ) quit = cl_aunit_assert=>no ).
    cl_aunit_assert=>assert_equals( exp = 'CM'   act = mo_ref->calculate_number_symbol( 900 ) quit = cl_aunit_assert=>no ).
    cl_aunit_assert=>assert_equals( exp = 'VIII' act = mo_ref->calculate_number_symbol( 8 )   quit = cl_aunit_assert=>no ).
  ENDMETHOD.

  METHOD test_get_interval.
    cl_aunit_assert=>assert_equals(
        exp = VALUE ycl_roman_numerals_converter=>ts_value_interval( min = 500 max = 1000 )
        act = mo_ref->get_value_interval( 900 )
        quit = cl_aunit_assert=>no ).

    cl_aunit_assert=>assert_equals(
        exp = VALUE ycl_roman_numerals_converter=>ts_value_interval( min = 100 max = 500 )
        act = mo_ref->get_value_interval( 300 )
        quit = cl_aunit_assert=>no ).
  ENDMETHOD.

  METHOD test_get_fraction.
    cl_aunit_assert=>assert_equals(
        exp = 'CM'
        act = mo_ref->map_number_to_numeral( number = 900 interval = VALUE #( min = 500 max = 1000 ) )
        quit = cl_aunit_assert=>no ).
    cl_aunit_assert=>assert_equals(
        exp = 'VIII'
        act = mo_ref->map_number_to_numeral( number = 8 interval = VALUE #( min = 5 max = 10 ) )
        quit = cl_aunit_assert=>no ).
    cl_aunit_assert=>assert_equals(
        exp = 'CCC'
        act = mo_ref->map_number_to_numeral( number = 300 interval = VALUE #( min = 100 max = 500 ) )
        quit = cl_aunit_assert=>no ).
  ENDMETHOD.

  METHOD test_get_sign_multiplicator.
    cl_aunit_assert=>assert_equals( exp = -1  act = mo_ref->get_sign_multiplicator( 4 ) quit = cl_aunit_assert=>no ).
    cl_aunit_assert=>assert_equals( exp = 1   act = mo_ref->get_sign_multiplicator( 3 ) quit = cl_aunit_assert=>no ).
  ENDMETHOD.

  METHOD test_conver_number.
    cl_aunit_assert=>assert_equals( exp = 'MCDXLIX'     act = mo_ref->convert_number( 1449 ) quit = cl_aunit_assert=>no ).
    cl_aunit_assert=>assert_equals( exp = 'MMMMDCXCIX'  act = mo_ref->convert_number( 4699 ) quit = cl_aunit_assert=>no ).
    cl_aunit_assert=>assert_equals( exp = 'MCMLXXXI'    act = mo_ref->convert_number( 1981 ) quit = cl_aunit_assert=>no ).
    cl_aunit_assert=>assert_equals( exp = 'III'         act = mo_ref->convert_number( 3 )    quit = cl_aunit_assert=>no ).
    cl_aunit_assert=>assert_equals( exp = 'V'           act = mo_ref->convert_number( 5 )    quit = cl_aunit_assert=>no ).
    cl_aunit_assert=>assert_equals( exp = 'IV'          act = mo_ref->convert_number( 4 )    quit = cl_aunit_assert=>no ).
    cl_aunit_assert=>assert_equals( exp = 'VI'          act = mo_ref->convert_number( 6 )    quit = cl_aunit_assert=>no ).
  ENDMETHOD.

ENDCLASS.
