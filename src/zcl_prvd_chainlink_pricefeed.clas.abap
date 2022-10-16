CLASS zcl_prvd_chainlink_pricefeed DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.


PUBLIC SECTION.
 INTERFACES zif_prvd_chainlink_pricefeed .

PROTECTED SECTION.
    data: lo_prvd_nchain_helper type ref to zcl_proubc_nchain_helper.
PRIVATE SECTION.
ENDCLASS.



CLASS zcl_prvd_chainlink_pricefeed IMPLEMENTATION.
  METHOD zif_prvd_chainlink_pricefeed~call_chainlink_pricefeeds.

  ENDMETHOD.

  METHOD zif_prvd_chainlink_pricefeed~generate_s4_market_rate_file.

  ENDMETHOD.

  METHOD zif_prvd_chainlink_pricefeed~prvd_authenticate.
  "get an access token in one of 3 ways: basic auth, refresh token linked to the current SAP user, or temporary id

  case iv_authtype.
    when 'B'. "basic auth
    when 'R'. "refresh token - most preferred mechanism
    when 'T'. "temporary ident + nchain wallet.
  endcase.
  ENDMETHOD.

  METHOD zif_prvd_chainlink_pricefeed~read_market_rate_file.

  ENDMETHOD.

  METHOD zif_prvd_chainlink_pricefeed~update_s4hana_market_rates.
    DATA: lt_updated_market_rates TYPE TABLE OF tcurr.

    "note TB_DATAFEED_MODIFY_ALL also calls this ... and interest rate, spreads, and other market data
    CALL FUNCTION 'TB_DATAFEED_MODIFY_TCURR'
      TABLES
        mod_tcurr = lt_updated_market_rates .
    IF sy-subrc NE 0.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
