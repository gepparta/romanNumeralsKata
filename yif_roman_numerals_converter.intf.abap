interface YIF_ROMAN_NUMERALS_CONVERTER
  public .


  methods CONVERT_NUMERAL
    importing
      !NUMERAL type STRING
    returning
      value(VALUE) type I .
  methods CONVERT_NUMBER
    importing
      !NUMBER type I
    returning
      value(RESULT) type STRING .
endinterface.
