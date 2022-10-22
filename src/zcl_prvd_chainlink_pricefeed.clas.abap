CLASS zcl_prvd_chainlink_pricefeed DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.


PUBLIC SECTION.
 INTERFACES zif_prvd_chainlink_pricefeed .

 CLASS-METHODS: factory EXPORTING eo_prvd_chainlink_pricefeed TYPE REF TO zcl_prvd_chainlink_pricefeed.

PROTECTED SECTION.
    DATA: lo_prvd_nchain_helper TYPE REF TO zcl_proubc_nchain_helper,
          lt_selected_contracts TYPE TABLE OF zprvdpricefeed.
    METHODS: authenticate_basic IMPORTING iv_prvduser TYPE string
                                          iv_prvduserpw TYPE string,
             authenticate_temp,
             authenticate_token,
             select_pricefeed_contracts,
             generate_pricefeed_contract IMPORTING is_selected_pricefeed TYPE zprvdpricefeed,
             get_chainlink_marketrate_files,
             update_from_marketrate_file,
             execute_pricefeed_contract EXPORTING es_pricefeed_result TYPE zif_prvd_chainlink_pricefeed=>ty_chainlink_pricefeed_result.
PRIVATE SECTION.
    METHODS: get_nchain_helper EXPORTING eo_prvd_nchain_helper TYPE REF TO zcl_proubc_nchain_helper.
ENDCLASS.

CLASS zcl_prvd_chainlink_pricefeed IMPLEMENTATION.

  METHOD factory.
    DATA: lo_prvd_chainlink_pricefeed TYPE REF TO zcl_prvd_chainlink_pricefeed.
    lo_prvd_chainlink_pricefeed = NEW zcl_prvd_chainlink_pricefeed( ).
    eo_prvd_chainlink_pricefeed = lo_prvd_chainlink_pricefeed.
  ENDMETHOD.

  METHOD authenticate_basic.
    me->get_nchain_helper( ).
    "lo_prvd_nchain_helper-> "TODO add the basic auth mechanism if absolutely needed
  ENDMETHOD.

  METHOD authenticate_temp.
    me->get_nchain_helper( ).
    "TODO add the temp auth mechanism
  ENDMETHOD.

  METHOD authenticate_token.
    DATA: lv_tenant TYPE zprvdtenantid,
          lv_subj_acct TYPE zprvdtenantid,
          lv_workgroup_id TYPE zprvdtenantid.

    GET PARAMETER ID 'ZPRVDTENANT' FIELD lv_tenant.
    GET PARAMETER ID 'ZPRVDSUBJACCTID' FIELD lv_subj_acct.
    GET PARAMETER ID 'ZPRVDWRKGRP' FIELD lv_workgroup_id.
    me->get_nchain_helper( ).
    lo_prvd_nchain_helper->call_ident_api(
      EXPORTING
        iv_tenant      = lv_tenant
        iv_subjacct    = lv_subj_acct
        "iv_workgrp     = lv_workgroup_id todo add this to core
*     IMPORTING
*        ev_authtoken   =
*        status         =
*        ev_bpiendpoint =
*    ).
    ).
  ENDMETHOD.

  METHOD select_pricefeed_contracts.
    SELECT * FROM zprvdpricefeed INTO TABLE lt_selected_contracts.
  ENDMETHOD.

  METHOD generate_pricefeed_contract.
  ENDMETHOD.

  METHOD get_chainlink_marketrate_files.
  ENDMETHOD.

  METHOD zif_prvd_chainlink_pricefeed~call_chainlink_pricefeeds.

    DATA: lv_sap_chainlink_timestampl TYPE timestampl,
          ls_pricefeed_result TYPE zif_prvd_chainlink_pricefeed=>ty_chainlink_pricefeed_result,
          lt_pricefeed_results TYPE zif_prvd_chainlink_pricefeed=>tty_pricefeed_results.

    GET TIME STAMP FIELD lv_sap_chainlink_timestampl.

    me->select_pricefeed_contracts( ).
    LOOP AT lt_selected_contracts ASSIGNING FIELD-SYMBOL(<fs_selected_contract>).
        CLEAR: ls_pricefeed_result.
        me->generate_pricefeed_contract( EXPORTING is_selected_pricefeed = <fs_selected_contract> ).
        me->execute_pricefeed_contract( IMPORTING es_pricefeed_result = ls_pricefeed_result ).
        APPEND ls_pricefeed_result TO lt_pricefeed_results.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_prvd_chainlink_pricefeed~generate_s4_market_rate_file.

  ENDMETHOD.

  METHOD zif_prvd_chainlink_pricefeed~prvd_authenticate.
  "get an access token in one of 3 ways: basic auth, refresh token linked to the current SAP user, or temporary id

      CASE iv_authtype.
        WHEN 'B'. "basic auth
            me->authenticate_basic( EXPORTING iv_prvduser = iv_prvduser iv_prvduserpw = iv_prvduserpw  ).
        WHEN 'R'. "refresh token - most preferred mechanism
            me->authenticate_token( ).
        WHEN 'T'. "temporary ident + nchain wallet.
            me->authenticate_temp(  ).
      ENDCASE.
  ENDMETHOD.

  METHOD zif_prvd_chainlink_pricefeed~read_market_rate_file.
    DATA: lt_pricefeed_results TYPE zif_prvd_chainlink_pricefeed=>tty_pricefeed_results,
          ls_tcurr_entry TYPE tcurr,
          lt_tcurr_entries TYPE TABLE OF tcurr.

          "get the pricefeed files and parse them into pricefeed results

          LOOP AT lt_pricefeed_results ASSIGNING FIELD-SYMBOL(<fs_pricefeed_results>).
            CLEAR ls_tcurr_entry.
            ls_tcurr_entry-fcurr = <fs_pricefeed_results>-from_currency.
            "ls_tcurr_entry-ffact "TODO from account units e.g $1 = 1000 satoshi etc
            ls_tcurr_entry-gdatu = sy-datum.
            "ls_tcurr_entry-kurst "TODO map exchange rate type
            ls_tcurr_entry-tcurr = <fs_pricefeed_results>-to_currency.
            "ls_tcurr_entry-tfact = TODO to account units 1000 satoshi = $1 etc
            "ls_tcurr_entry-ukurs "TODO mark exchange rate
            APPEND ls_tcurr_entry TO lt_tcurr_entries.
          ENDLOOP.

  ENDMETHOD.

  METHOD zif_prvd_chainlink_pricefeed~archive_files.
               "iv_directorylocation
               "iv_archivelocation
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

  METHOD update_from_marketrate_file.
    "me.
  ENDMETHOD.

  METHOD execute_pricefeed_contract.
    data: ls_pricefeed_result TYPE zif_prvd_chainlink_pricefeed=>ty_chainlink_pricefeed_result.
          es_pricefeed_result = ls_pricefeed_result.
  ENDMETHOD.

  METHOD get_nchain_helper.
    IF lo_prvd_nchain_helper IS BOUND.
        eo_prvd_nchain_helper = lo_prvd_nchain_helper.
    ELSE.
        lo_prvd_nchain_helper = NEW zcl_proubc_nchain_helper( ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.
