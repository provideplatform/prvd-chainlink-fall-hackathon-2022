CLASS zcl_prvd_chainlink_pricefeed DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.


  PUBLIC SECTION.
    INTERFACES zif_prvd_chainlink_pricefeed .

    CLASS-METHODS: factory EXPORTING eo_prvd_chainlink_pricefeed TYPE REF TO zcl_prvd_chainlink_pricefeed.
    METHODS: setup IMPORTING iv_tenant       TYPE zprvdtenantid OPTIONAL
                             iv_subj_acct    TYPE zprvdtenantid OPTIONAL
                             iv_workgroup_id TYPE zprvdtenantid OPTIONAL
                             iv_do_baseline  TYPE char1 OPTIONAL
                             iv_do_ipfs      TYPE char1 OPTIONAL,
      format_eth_price IMPORTING !iv_chainlink_eth_price TYPE int8
                                 !iv_to_currency         TYPE waers_curc
                       EXPORTING !ev_sap_eth_price       TYPE ukurs
                                 !ev_sap_ffact           TYPE waers_curc
                                 !ev_sap_tfact           TYPE waers_curc,
      save_result     IMPORTING !is_pricefeedresult TYPE zif_prvd_chainlink_pricefeed=>ty_chainlink_pricefeed_result
                      EXPORTING !es_pf_table        TYPE zprvd_pf_results.

  PROTECTED SECTION.
    DATA: lo_prvd_api_helper    TYPE REF TO zcl_proubc_api_helper,
          lo_prvd_nchain_helper TYPE REF TO zcl_proubc_nchain_helper,
          lt_selected_contracts TYPE TABLE OF zprvdpricefeed,
          lv_tenant             TYPE zprvdtenantid,
          lv_subj_acct          TYPE zprvdtenantid,
          lv_workgroup_id       TYPE zprvdtenantid,
          lv_do_baseline        type char1,
          lv_do_ipfs            type char1.
    METHODS: authenticate_basic IMPORTING iv_prvduser   TYPE string
                                          iv_prvduserpw TYPE string,
      authenticate_temp,
      authenticate_token,
      select_pricefeed_contracts,
      generate_pricefeed_contract IMPORTING is_selected_pricefeed     TYPE zprvdpricefeed
                                  EXPORTING es_pricefeed_contract_req TYPE zif_proubc_nchain=>ty_chainlinkpricefeed_req,
      get_chainlink_marketrate_files,
      update_from_marketrate_file,
      execute_pricefeed_contract EXPORTING es_pricefeed_result TYPE zif_prvd_chainlink_pricefeed=>ty_chainlink_pricefeed_result,
      parse_latestround IMPORTING is_execute_contract_resp TYPE zif_proubc_nchain=>ty_executecontract_resp
                        EXPORTING es_latestround           TYPE zif_prvd_chainlink_pricefeed=>ty_latestrounddata_result,
      map_latestround_to_result IMPORTING is_latestround           TYPE zif_prvd_chainlink_pricefeed=>ty_latestrounddata_result
                                          is_execute_contract_resp TYPE zif_proubc_nchain=>ty_executecontract_resp
                                          iv_from_currency         TYPE waers_curc
                                          iv_to_currency           TYPE waers_curc
                                EXPORTING es_pricefeedresult       TYPE zif_prvd_chainlink_pricefeed=>ty_chainlink_pricefeed_result.
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
                          IMPORTING es_selectedcontract = ls_selectedcontract ).
    es_pricefeed_contract_req = ls_selectedcontract.
  ENDMETHOD.

  METHOD get_chainlink_marketrate_files.
  ENDMETHOD.

  METHOD zif_prvd_chainlink_pricefeed~call_chainlink_pricefeeds.

    DATA: lv_sap_chainlink_timestampl TYPE timestampl,
          ls_execute_contract_resp    TYPE zif_proubc_nchain=>ty_executecontract_resp,
          ls_latestround              TYPE zif_prvd_chainlink_pricefeed=>ty_latestrounddata_result,
          ls_pricefeed_result         TYPE zif_prvd_chainlink_pricefeed=>ty_chainlink_pricefeed_result,
          lt_pricefeed_results        TYPE zif_prvd_chainlink_pricefeed=>tty_pricefeed_results,
          lv_from_currency            TYPE waers_curc,
          lv_to_currency              TYPE waers_curc,
          lt_bpi_obj                  type zbpiobj.

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
        iv_from_currency = lv_from_currency
        iv_to_currency = lv_to_currency
      IMPORTING
        es_pricefeedresult = ls_pricefeed_result
    ).
    DATA: ls_pf_table_result TYPE zprvd_pf_results,
          lt_tcurr type ftdf_tab_tcurr.
    me->save_result( EXPORTING is_pricefeedresult = ls_pricefeed_result
                     IMPORTING es_pf_table  = ls_pf_table_result ).
    me->zif_prvd_chainlink_pricefeed~format_to_market_rates( exporting it_pf_results = ls_pf_table_result
                                                             IMPORTING et_tcurr = lt_tcurr ).
    me->zif_prvd_chainlink_pricefeed~update_s4hana_market_rates( exporting it_tcurr = lt_tcurr ).
    if lv_do_baseline is not INITIAL.
        me->zif_prvd_chainlink_pricefeed~emit_baseline_zkp_msg(
          EXPORTING
            is_pricefeed_result =  ls_pf_table_result
          IMPORTING
            es_bpiobj           = lt_bpi_obj
        ).
    endif.
    if lv_do_ipfs is not initial.
        "me->zif_prvd_chainlink_pricefeed~generate_s4_market_rate_file(  ) todo change params
        "me->zif_prvd_chainlink_pricefeed~move_file_to_ipfs(  )
    endif.
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
          ls_tcurr_entry       TYPE tcurr,
          lt_tcurr_entries     TYPE TABLE OF tcurr.

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

  METHOD zif_prvd_chainlink_pricefeed~format_to_market_rates.
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
        mod_tcurr = lt_updated_market_rates.
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
    lv_do_baseline = iv_do_baseline.
    lv_do_ipfs = iv_do_ipfs.
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

    DATA: ls_latestround  TYPE zif_prvd_chainlink_pricefeed=>ty_latestrounddata_result,
          ls_response_tab TYPE REF TO data.
    FIELD-SYMBOLS: <fs_responses>       TYPE any,
                   <fs_t_responses>     TYPE ANY TABLE,
                   <fs_roundid>         TYPE char80,
                   <fs_answer>          TYPE char256,
                   <fs_startedat>       TYPE char256,
                   <fs_updatedat>       TYPE char256,
                   <fs_answeredinround> TYPE char80.
    DATA: lo_roundid         TYPE REF TO data,
          lo_answer          TYPE REF TO data,
          lo_startedat       TYPE REF TO data,
          lo_updatedat       TYPE REF TO data,
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
          ASSIGN lo_startedat->* TO FIELD-SYMBOL(<fs_startedat_raw>).
          DATA: lv_startedat_raw TYPE REF TO data.
          lv_startedat_raw = <fs_startedat_raw>.
          ASSIGN lv_startedat_raw->* TO FIELD-SYMBOL(<fs_startedat_raw2>).
          ls_latestround-startedat = <fs_startedat_raw2>.
        WHEN 4.
          GET REFERENCE OF <fs_index> INTO lo_updatedat.
          ASSIGN lo_updatedat->* TO FIELD-SYMBOL(<fs_updatedat_raw>).
          DATA: lv_updatedat_raw TYPE REF TO data.
          lv_updatedat_raw = <fs_updatedat_raw>.
          ASSIGN lv_updatedat_raw->* TO FIELD-SYMBOL(<fs_updatedat_raw2>).
          ls_latestround-updatedat = <fs_updatedat_raw2>.
        WHEN 5.
          GET REFERENCE OF <fs_index> INTO lo_answeredinround.
          ASSIGN lo_answeredinround->* TO FIELD-SYMBOL(<fs_answeredinround_raw>).
          DATA: lv_answeredinround_raw TYPE REF TO data.
          lv_answeredinround_raw = <fs_answeredinround_raw>.
          ASSIGN lv_answeredinround_raw->* TO FIELD-SYMBOL(<fs_answeredinround_raw2>).
          ls_latestround-answeredinround = <fs_answeredinround_raw2>.
        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.
    es_latestround = ls_latestround.
  ENDMETHOD.

  METHOD map_latestround_to_result.
    DATA: ls_result TYPE zif_prvd_chainlink_pricefeed=>ty_chainlink_pricefeed_result.

    me->format_eth_price(
      EXPORTING
        iv_chainlink_eth_price =  is_latestround-answer
        iv_to_currency         =  iv_to_currency
      IMPORTING
        ev_sap_eth_price       = ls_result-formatted_amount
        ev_sap_ffact           = ls_result-from_currency
        ev_sap_tfact           = ls_result-to_currency
    ).
    ls_result-user_responsible = sy-uname.
    "LS_RESULT-walletid =


  ENDMETHOD.

  METHOD format_eth_price.
    DATA: lv_eth_formatted_price TYPE p DECIMALS 5, "9 character decimal type + up to 5 decimal places
          lv_eth_floated_price   TYPE float,
          lv_float_factor        TYPE float.

    CASE iv_to_currency.
      WHEN 'USD'.
        lv_float_factor = '0.00000001'.
      WHEN OTHERS.
        lv_float_factor = '0.00000001'.
    ENDCASE.

    lv_eth_floated_price = iv_chainlink_eth_price * lv_float_factor.

    "to do get the right structures
    ev_sap_ffact = 'ETH'.
    ev_sap_tfact = iv_to_currency.


    lv_eth_formatted_price = lv_eth_floated_price.

    ev_sap_eth_price = lv_eth_formatted_price.
  ENDMETHOD.

  METHOD zif_prvd_chainlink_pricefeed~emit_baseline_zkp_msg.
    DATA: ls_protocol_msg_req  TYPE zif_proubc_baseline=>protocolmessage_req,
          ls_protocol_msg_resp TYPE zif_proubc_baseline=>protocolmessage_resp,
          lv_status            TYPE i,
          lv_apiresponse       TYPE REF TO data,
          lv_apiresponsestr    TYPE string,
          lt_updatedbpis       TYPE TABLE OF zbpiobj,
          lt_newbpis           TYPE TABLE OF zbpiobj,
          lt_final_updatedbpis TYPE TABLE OF zbpiobj,
          lt_final_newbpis     TYPE TABLE OF zbpiobj,
          ls_pf_result_data    TYPE REF TO data,
          lv_daily_pf_key      TYPE string.

    GET REFERENCE OF is_pricefeed_result INTO ls_pf_result_data.

    "request to /api/v1/protocol_messages
    ls_protocol_msg_req-payload = ls_pf_result_data.
    ls_protocol_msg_req-payload_mimetype = 'json'.
    ls_protocol_msg_req-type = 'ZPRVD_PF_RESULTS'. "should be orders05 for demo purposes

    CONCATENATE is_pricefeed_result-kurst
                is_pricefeed_result-fcurr
                is_pricefeed_result-tcurr
                is_pricefeed_result-gdatu
    INTO lv_daily_pf_key SEPARATED BY '|'.
    ls_protocol_msg_req-id = lv_daily_pf_key.

    lo_prvd_api_helper->send_protocol_msg( EXPORTING body = ls_protocol_msg_req IMPORTING statuscode = lv_status
                                                                                    apiresponse = lv_apiresponse
                                                                                    apiresponsestr = lv_apiresponsestr  ). "should return 202

    IF lv_status = '202'.
      /ui2/cl_json=>deserialize( EXPORTING json = lv_apiresponsestr  CHANGING data = ls_protocol_msg_resp ).

      DATA: wa_bpiobj    TYPE zbpiobj,
            lv_timestamp TYPE timestampl.
      CLEAR: wa_bpiobj.
      SELECT SINGLE * FROM zbpiobj INTO wa_bpiobj WHERE object_id = ls_protocol_msg_req-id
                                                    AND baseline_id = ls_protocol_msg_resp-baseline_id.
      IF sy-subrc = 0.
        wa_bpiobj-baseline_id = ls_protocol_msg_resp-baseline_id. "To be provided by api
        wa_bpiobj-proof = ls_protocol_msg_resp-proof. "To be provided by api
        "wa_bpiobj-status = ls_protocol_msg_resp-. "To be determined by api response
        wa_bpiobj-object_id = ls_protocol_msg_req-id.
        wa_bpiobj-changed_by = sy-uname.
        wa_bpiobj-changed_at = lv_timestamp.
        wa_bpiobj-schematype = 'DDIC'.
        wa_bpiobj-schema_id = 'ZPRVD_PF_RESULTS'.
        wa_bpiobj-workgroup_id = ls_protocol_msg_resp-workgroup_id.
        wa_bpiobj-subject_account_id = ls_protocol_msg_resp-subject_account_id.
        APPEND wa_bpiobj TO lt_updatedbpis.
      ELSE.
        GET TIME STAMP FIELD lv_timestamp.
        wa_bpiobj-baseline_id = ls_protocol_msg_resp-baseline_id. "To be provided by api
        wa_bpiobj-proof = ls_protocol_msg_resp-proof. "To be provided by api
        "wa_bpiobj-status = ''. "To be determined by api response
        wa_bpiobj-object_id = ls_protocol_msg_req-id.
        wa_bpiobj-created_by = sy-uname.
        wa_bpiobj-created_at = lv_timestamp.
        wa_bpiobj-schematype = 'DDIC'.
        wa_bpiobj-schema_id = 'ZPRVD_PF_RESULTS'.
        wa_bpiobj-workgroup_id = ls_protocol_msg_resp-workgroup_id.
        wa_bpiobj-subject_account_id = ls_protocol_msg_resp-subject_account_id.
        APPEND wa_bpiobj TO lt_newbpis.
      ENDIF.
    ELSE. "log error message
    ENDIF.

    zcl_proubc_busobjhlpr=>validate_object_create(
      EXPORTING
        it_objects = lt_newbpis
      IMPORTING
        et_objects = lt_final_newbpis
    ).
    zcl_proubc_busobjhlpr=>create_object(
      EXPORTING
        it_objects = lt_final_newbpis
*      IMPORTING
*        et_objects =
    ).
  ENDMETHOD.

  METHOD  zif_prvd_chainlink_pricefeed~move_file_to_ipfs.
  ENDMETHOD.

  METHOD save_result.
    DATA: ls_prvd_pf_results TYPE zprvd_pf_results,
          lt_prvd_pf_results TYPE TABLE OF zprvd_pf_results.
    "map to to db structure
    ls_prvd_pf_results-fcurr = is_pricefeedresult-from_currency.
    ls_prvd_pf_results-gdatu = sy-datum. " todo add to is_pricefeedresult
    "ls_prvd_pf_results-kurst = is_pricefeedresult- todo add exchanges
    ls_prvd_pf_results-networkid = is_pricefeedresult-networkid.
    ls_prvd_pf_results-smartcontractaddress = is_pricefeedresult-smartcontractaddress.
    ls_prvd_pf_results-tcurr = is_pricefeedresult-to_currency.
    ls_prvd_pf_results-txn_hash = is_pricefeedresult-txn_hash.
    ls_prvd_pf_results-txn_processed_at = is_pricefeedresult-txn_processed_at.
    ls_prvd_pf_results-user_responsible = is_pricefeedresult-user_responsible.
    ls_prvd_pf_results-walletid = is_pricefeedresult-walletid.

    APPEND ls_prvd_pf_results TO lt_prvd_pf_results.
    es_pf_table = ls_prvd_pf_results.
    CLEAR ls_prvd_pf_results.
    MODIFY zprvd_pf_results FROM TABLE lt_prvd_pf_results.

  ENDMETHOD.

ENDCLASS.
