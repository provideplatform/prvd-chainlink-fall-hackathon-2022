CLASS zcl_prvd_chainlink_pricefeed DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.


PUBLIC SECTION.
 INTERFACES zif_prvd_chainlink_pricefeed .

 CLASS-METHODS: factory EXPORTING eo_prvd_chainlink_pricefeed TYPE REF TO zcl_prvd_chainlink_pricefeed.
 METHODS: setup IMPORTING Iv_tenant TYPE zprvdtenantid OPTIONAL
                          Iv_subj_acct TYPE zprvdtenantid OPTIONAL
                          Iv_workgroup_id TYPE zprvdtenantid OPTIONAL.

PROTECTED SECTION.
    DATA: lo_prvd_api_helper TYPE REF TO zcl_proubc_api_helper,
          lo_prvd_nchain_helper TYPE REF TO zcl_proubc_nchain_helper,
          lt_selected_contracts TYPE TABLE OF zprvdpricefeed,
          lv_tenant TYPE zprvdtenantid,
          lv_subj_acct TYPE zprvdtenantid,
          lv_workgroup_id TYPE zprvdtenantid.
    METHODS: authenticate_basic IMPORTING iv_prvduser TYPE string
                                          iv_prvduserpw TYPE string,
             authenticate_temp,
             authenticate_token,
             select_pricefeed_contracts,
             generate_pricefeed_contract IMPORTING is_selected_pricefeed TYPE zprvdpricefeed
                                         EXPORTING es_pricefeed_contract_req TYPE zif_proubc_nchain=>ty_chainlinkpricefeed_req,
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
    IF lv_tenant IS INITIAL.
    GET PARAMETER ID 'ZPRVDTENANT' FIELD lv_tenant.
    ENDIF.
    IF lv_subj_acct IS INITIAL.
    GET PARAMETER ID 'ZPRVDSUBJACCTID' FIELD lv_subj_acct.
    ENDIF.
    IF lv_workgroup_id IS INITIAL.
    GET PARAMETER ID 'ZPRVDWRKGRP' FIELD lv_workgroup_id.
    ENDIF.
    lo_prvd_api_helper = NEW zcl_proubc_api_helper( iv_tenant = lv_tenant iv_subject_acct_id = lv_subj_acct iv_workgroup_id = lv_workgroup_id ).
    lo_prvd_api_helper->call_ident_api(
      EXPORTING
        iv_tenant      = lv_tenant
        iv_subjacct    = lv_subj_acct
        "iv_workgrp     = lv_workgroup_id todo add this to core
*     IMPORTING
*        ev_authtoken   =
*        status         =
*        ev_bpiendpoint =
    ).
    lo_prvd_api_helper->get_nchain_helper( IMPORTING eo_prvd_nchain_helper = lo_prvd_nchain_helper ).

  ENDMETHOD.

  METHOD select_pricefeed_contracts.
    SELECT * FROM zprvdpricefeed INTO TABLE lt_selected_contracts.
  ENDMETHOD.

  METHOD generate_pricefeed_contract.
    DATA: ls_selectedcontract TYPE zif_proubc_nchain=>ty_chainlinkpricefeed_req.
          me->get_nchain_helper( ).
          DATA: lv_pricefeedname TYPE string.
                CONCATENATE is_selected_pricefeed-currency1 '/' is_selected_pricefeed-currency2 INTO lv_pricefeedname.
          me->lo_prvd_nchain_helper->smartcontract_factory(  EXPORTING iv_smartcontractaddress = is_selected_pricefeed-zprvdsmartcontractaddr
                                          iv_name                 = lv_pricefeedname
                                          iv_contract             = '' "this is more complex
                                          iv_walletaddress        = '' "from the wallet we created earlier
                                          iv_nchain_networkid     = is_selected_pricefeed-zprvdnchainnetworkid "goerli testnet nchain id, check if this always same
                                          iv_contracttype         = 'price-feed'
                                IMPORTING es_selectedContract = ls_selectedcontract ).
         es_pricefeed_contract_req = ls_selectedcontract.
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
    DATA: ls_pricefeed_result TYPE zif_prvd_chainlink_pricefeed=>ty_chainlink_pricefeed_result.
          es_pricefeed_result = ls_pricefeed_result.

*    me->lo_prvd_nchain_helper->lo_nchain_api->zif_proubc_nchain~createpricefeedcontract(
*      EXPORTING
*        is_pricefeedcontract = ls_selectedcontract
*      IMPORTING
*        ev_apiresponsestr    = lv_createdcontract_str
*        ev_apiresponse       = lv_createdcontract_data
*        ev_httpresponsecode  = lv_createdcontract_responsecd
**    ).
*    CASE lv_createdcontract_responsecd.
*      WHEN 202.
*      WHEN OTHERS.
*    ENDCASE.
**
**    me->lo_nchain_api->zif_proubc_nchain~executecontract(
**      EXPORTING
**        iv_contract_id      = '0xD4a33860578De61DBAbDc8BFdb98FD742fA7028e'
**        is_execcontractreq  = ls_executecontract
**      IMPORTING
**        ev_apiresponsestr   = lv_executecontract_str
**        ev_apiresponse      =  lv_executecontract_data
**        ev_httpresponsecode =  lv_executecontract_responsecd
**    ).
*    CASE lv_executecontract_responsecd.
*        WHEN 202.
*        WHEN OTHERS.
*    ENDCASE.



  ENDMETHOD.

  METHOD get_nchain_helper.
    IF lo_prvd_nchain_helper IS BOUND.
        eo_prvd_nchain_helper = lo_prvd_nchain_helper.
    ELSE.
        lo_prvd_nchain_helper = NEW zcl_proubc_nchain_helper( ).
    ENDIF.
  ENDMETHOD.

  METHOD setup.
    lv_tenant = iv_tenant.
    lv_subj_acct = iv_subj_acct.
    lv_workgroup_id = iv_workgroup_id.
  ENDMETHOD.

ENDCLASS.
