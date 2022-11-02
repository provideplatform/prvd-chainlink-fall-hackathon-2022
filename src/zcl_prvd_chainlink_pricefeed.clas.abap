CLASS zcl_prvd_chainlink_pricefeed DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.


PUBLIC SECTION.
 INTERFACES zif_prvd_chainlink_pricefeed .

 CLASS-METHODS: factory EXPORTING eo_prvd_chainlink_pricefeed TYPE REF TO zcl_prvd_chainlink_pricefeed.
 METHODS: setup IMPORTING Iv_tenant TYPE zprvdtenantid OPTIONAL
                          Iv_subj_acct TYPE zprvdtenantid OPTIONAL
                          Iv_workgroup_id TYPE zprvdtenantid OPTIONAL,
          format_eth_price IMPORTING !iv_chainlink_eth_price TYPE i
                           EXPORTING !ev_sap_eth_price TYPE ukurs
                                     !ev_sap_ffact     TYPE ffact
                                     !ev_sap_tfact     TYPE tfact.

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
             execute_pricefeed_contract EXPORTING es_pricefeed_result TYPE zif_prvd_chainlink_pricefeed=>ty_chainlink_pricefeed_result,
             parse_latestround IMPORTING is_execute_contract_resp TYPE zif_proubc_nchain=>ty_executecontract_resp
                                    EXPORTING es_latestround TYPE zif_prvd_chainlink_pricefeed=>ty_latestrounddata_result,
             map_latestround_to_result IMPORTING is_latestround TYPE zif_prvd_chainlink_pricefeed=>ty_latestrounddata_result
                                                 Is_execute_contract_resp TYPE zif_proubc_nchain=>ty_executecontract_resp
                                       EXPORTING es_pricefeedresult TYPE zif_prvd_chainlink_pricefeed=>ty_chainlink_pricefeed_result.
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
          ls_execute_contract_resp TYPE zif_proubc_nchain=>ty_executecontract_resp,
          ls_latestround TYPE zif_prvd_chainlink_pricefeed=>ty_latestrounddata_result,
          ls_pricefeed_result TYPE zif_prvd_chainlink_pricefeed=>ty_chainlink_pricefeed_result,
          lt_pricefeed_results TYPE zif_prvd_chainlink_pricefeed=>tty_pricefeed_results.

    GET TIME STAMP FIELD lv_sap_chainlink_timestampl.

    lo_prvd_nchain_helper->call_chainlink_pricefeed(
      EXPORTING
        iv_inputcurrency  = 'ETH'
        iv_inputamount    = '1'
        iv_outputcurrency = 'USD'
      IMPORTING
        es_contract_resp = ls_execute_contract_resp
*        ev_outputamount   =
    ).
    me->parse_latestround(
    EXPORTING
        is_execute_contract_resp = ls_execute_contract_resp
      IMPORTING
        es_latestround = ls_latestround
    ).
    me->map_latestround_to_result(
      EXPORTING
        is_latestround     = ls_latestround
        is_execute_contract_resp = ls_execute_contract_resp
      IMPORTING
        es_pricefeedresult = ls_pricefeed_result
    ).
" start using this after first successful test
*    me->select_pricefeed_contracts( ).
*    LOOP AT lt_selected_contracts ASSIGNING FIELD-SYMBOL(<fs_selected_contract>).
*        CLEAR: ls_pricefeed_result.
*        me->generate_pricefeed_contract( EXPORTING is_selected_pricefeed = <fs_selected_contract> ).
*        me->execute_pricefeed_contract( IMPORTING es_pricefeed_result = ls_pricefeed_result ).
*        APPEND ls_pricefeed_result TO lt_pricefeed_results.
*    ENDLOOP.

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

  METHOD parse_latestround.

* response values are as specified in the as exact order as in returns
*       function latestRoundData()
*    public
*    view
*    virtual
*    override
*    returns (
*      uint80 roundId,
*      int256 answer, <-- this is your price, still needs some unit conversions for SAP
*      uint256 startedAt,
*      uint256 updatedAt,
*      uint80 answeredInRound
*    ) "log the other data - important refere

  DATA: ls_latestround TYPE zif_prvd_chainlink_pricefeed=>ty_latestrounddata_result,
        ls_response_tab TYPE REF TO data.
  FIELD-SYMBOLS: <fs_responses> TYPE any,
                <fs_t_responses> TYPE ANY TABLE,
                <fs_roundid> TYPE char80,
                <fs_answer> TYPE char256,
                <fs_startedat> TYPE char256,
                <fs_updatedat> TYPE char256,
                <fs_answeredinround> TYPE char80.
  DATA: lo_roundid TYPE REF TO data,
        lo_answer TYPE REF TO data,
        lo_startedat TYPE REF TO data,
        lo_updatedat TYPE REF TO data,
        lo_answeredinround TYPE REF TO data.

  ASSIGN COMPONENT 'RESPONSE' OF STRUCTURE is_execute_contract_resp  TO <fs_responses>.
  ls_response_tab = <fs_responses>.
  ASSIGN ls_response_tab->* TO <fs_t_responses>.
  LOOP AT <fs_t_responses> ASSIGNING FIELD-SYMBOL(<fs_index>).
    CASE sy-tabix.
    WHEN 1.
        GET REFERENCE OF <fs_index> INTO lo_roundid.
        ASSIGN lo_roundid->* TO FIELD-SYMBOL(<fs_roundid_raw>).
        DATA: lv_roundid_raw TYPE REF TO data.
        lv_roundid_raw = <fs_roundid_raw>.
        ASSIGN lv_roundid_raw->* TO FIELD-SYMBOL(<fs_roundid_raw2>).
        "<fs_roundid> = <fs_roundid_raw>.
        ls_latestround-roundid = <fs_roundid_raw2>.
    WHEN 2.
        GET REFERENCE OF <fs_index> INTO lo_answer.
        ASSIGN lo_answer->* TO FIELD-SYMBOL(<fs_answer_raw>).
        DATA: lv_answer_raw TYPE REF TO data.
        lv_answer_raw = <fs_answer_raw>.
        ASSIGN lv_answer_raw->* TO FIELD-SYMBOL(<fs_answer_raw2>).
        ls_latestround-answer = <fs_answer_raw2>.
    WHEN 3.
        GET REFERENCE OF <fs_index> INTO lo_startedat.
        assign lo_startedat->* to FIELD-SYMBOL(<fs_startedat_raw>).
        data: lv_startedat_raw type ref to data.
        lv_startedat_raw = <fs_startedat_raw>.
        assign lv_startedat_raw->* to FIELD-SYMBOL(<fs_startedat_raw2>).
        ls_latestround-startedat = <fs_startedat_raw2>.
    WHEN 4.
        GET REFERENCE OF <fs_index> INTO lo_updatedat.
        assign lo_updatedat->* to FIELD-SYMBOL(<fs_updatedat_raw>).
        data: lv_updatedat_raw type ref to data.
        lv_updatedat_raw = <fs_updatedat_raw>.
        assign lv_updatedat_raw->* to FIELD-SYMBOL(<fs_updatedat_raw2>).
        ls_latestround-updatedat = <fs_updatedat_raw2>.
    WHEN 5.
        GET REFERENCE OF <fs_index> INTO lo_answeredinround.
        assign lo_answeredinround->* to FIELD-SYMBOL(<fs_answeredinround_raw>).
        data: lv_answeredinround_raw type ref to data.
        lv_answeredinround_raw = <fs_answeredinround_raw>.
        ASSign lv_answeredinround_raw->* to FIELD-SYMBOL(<fs_answeredinround_raw2>).
        ls_latestround-answeredinround = <fs_answeredinround_raw2>.
    WHEN OTHERS.
    ENDCASE.
  ENDLOOP.
    es_latestround = ls_latestround.
  ENDMETHOD.

  METHOD map_latestround_to_result.
  DATA: ls_result TYPE zif_prvd_chainlink_pricefeed=>ty_chainlink_pricefeed_result.

  ls_result-user_responsible = sy-uname.
  "LS_RESULT-walletid =


  ENDMETHOD.

  METHOD format_eth_price.
  ENDMETHOD.

ENDCLASS.
