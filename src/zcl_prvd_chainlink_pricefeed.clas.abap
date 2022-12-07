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
                             iv_do_ipfs      TYPE char1 OPTIONAL
                             iv_ipfsp        TYPE string OPTIONAL
                             iv_ipfsk        TYPE string OPTIONAL,
      format_eth_price IMPORTING !iv_chainlink_eth_price TYPE int8
                                 !iv_to_currency         TYPE waers_curc
                       EXPORTING !ev_sap_eth_price       TYPE ukurs_curr,
      format_btc_price IMPORTING !iv_chainlink_btc_price TYPE int8
                                 !iv_to_currency         TYPE waers_curc
                       EXPORTING !ev_sap_btc_price       TYPE ukurs_curr,
      save_result     IMPORTING !is_pricefeedresult TYPE zif_prvd_chainlink_pricefeed=>ty_chainlink_pricefeed_result
                      EXPORTING !et_pf_table        TYPE zif_prvd_chainlink_pricefeed=>tty_pf_result,
      run_pricefeed_batch IMPORTING iv_networkid    TYPE zprvd_nchain_networkid
                                    it_selected_pfs TYPE zproubc_pf_pairid_rt
                          EXPORTING et_zkps         TYPE zif_prvd_chainlink_pricefeed=>tty_bpiobj
                                    ev_al11_file    TYPE string
                                    ev_ipfs_cid     TYPE string.

  PROTECTED SECTION.
    DATA: lo_prvd_api_helper    TYPE REF TO zcl_proubc_api_helper,
          lo_prvd_nchain_helper TYPE REF TO zcl_proubc_nchain_helper,
          lt_selected_contracts TYPE TABLE OF zprvdpricefeed,
          lv_tenant             TYPE zprvdtenantid,
          lv_subj_acct          TYPE zprvdtenantid,
          lv_workgroup_id       TYPE zprvdtenantid,
          lv_ipfsprojid         TYPE string,
          lv_ipfsapikey         TYPE string,
          lv_do_baseline        TYPE char1,
          lv_do_ipfs            TYPE char1.
    METHODS: authenticate_basic IMPORTING iv_prvduser   TYPE string
                                          iv_prvduserpw TYPE string,
      authenticate_temp,
      authenticate_token,
      select_pricefeed_contracts,
      get_chainlink_marketrate_files,
      update_from_marketrate_file,

      parse_latestround IMPORTING is_execute_contract_resp TYPE zif_proubc_nchain=>ty_executecontract_resp
                        EXPORTING es_latestround           TYPE zif_prvd_chainlink_pricefeed=>ty_latestrounddata_result,
      map_latestround_to_result IMPORTING is_latestround              TYPE zif_prvd_chainlink_pricefeed=>ty_latestrounddata_result
                                          is_execute_contract_resp    TYPE zif_proubc_nchain=>ty_executecontract_resp
                                          is_execute_contract_summary TYPE zif_proubc_nchain=>ty_executecontract_summary
                                          iv_from_currency            TYPE waers_curc
                                          iv_to_currency              TYPE waers_curc
                                EXPORTING es_pricefeedresult          TYPE zif_prvd_chainlink_pricefeed=>ty_chainlink_pricefeed_result.
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
    lo_prvd_api_helper = NEW zcl_proubc_api_helper( iv_tenant = lv_tenant 
                                                    iv_subject_acct_id = lv_subj_acct
                                                    iv_workgroup_id = lv_workgroup_id ).
    lo_prvd_api_helper->call_ident_api(
      EXPORTING
        iv_tenant      = lv_tenant
        iv_subjacct    = lv_subj_acct
    ).
    lo_prvd_api_helper->get_nchain_helper( IMPORTING eo_prvd_nchain_helper = lo_prvd_nchain_helper ).

  ENDMETHOD.

  METHOD select_pricefeed_contracts.
    SELECT * FROM zprvdpricefeed INTO TABLE lt_selected_contracts.
  ENDMETHOD.

  METHOD get_chainlink_marketrate_files.
  ENDMETHOD.

  METHOD zif_prvd_chainlink_pricefeed~call_chainlink_pricefeeds.

    DATA: lv_sap_chainlink_timestampl TYPE timestampl,
          ls_execute_contract_resp    TYPE zif_proubc_nchain=>ty_executecontract_resp,
          ls_execute_contract_summary TYPE zif_proubc_nchain=>ty_executecontract_summary,
          ls_latestround              TYPE zif_prvd_chainlink_pricefeed=>ty_latestrounddata_result,
          ls_pricefeed_result         TYPE zif_prvd_chainlink_pricefeed=>ty_chainlink_pricefeed_result,
          lt_pricefeed_results        TYPE zif_prvd_chainlink_pricefeed=>tty_chainlink_pricefeed_result,
          lv_from_currency            TYPE waers_curc,
          lv_to_currency              TYPE waers_curc,
          lt_bpi_obj                  TYPE zif_prvd_chainlink_pricefeed=>tty_bpiobj.

    GET TIME STAMP FIELD lv_sap_chainlink_timestampl.

    lv_from_currency = 'ETH'.
    lv_to_currency = 'USD'.

    lo_prvd_nchain_helper->call_chainlink_pricefeed(
      EXPORTING
        iv_inputcurrency  = 'ETH'
        iv_inputamount    = '1'
        iv_outputcurrency = 'USD'
      IMPORTING
        es_contract_resp = ls_execute_contract_resp
        es_contract_summary = ls_execute_contract_summary
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
        is_execute_contract_summary = ls_execute_contract_summary
        iv_from_currency = lv_from_currency
        iv_to_currency = lv_to_currency
      IMPORTING
        es_pricefeedresult = ls_pricefeed_result
    ).
    DATA: ls_pf_table_result TYPE zif_prvd_chainlink_pricefeed=>tty_pf_result,
          lt_tcurr           TYPE ftdf_tab_tcurr.
    me->save_result( EXPORTING is_pricefeedresult = ls_pricefeed_result
                     IMPORTING et_pf_table  = ls_pf_table_result ).
    me->zif_prvd_chainlink_pricefeed~format_to_market_rates( EXPORTING it_pf_results = ls_pf_table_result
                                                             IMPORTING et_tcurr = lt_tcurr ).
    me->zif_prvd_chainlink_pricefeed~update_s4hana_market_rates( EXPORTING it_tcurr = lt_tcurr ).
    IF lv_do_baseline IS NOT INITIAL.
      me->zif_prvd_chainlink_pricefeed~emit_baseline_zkp_msg(
        EXPORTING
          is_pricefeed_result =  ls_pf_table_result
        IMPORTING
          es_bpiobj           = lt_bpi_obj
      ).
    ENDIF.
    IF lv_do_ipfs IS NOT INITIAL.
      "me->zif_prvd_chainlink_pricefeed~generate_s4_market_rate_file(  ) todo change params
      "me->zif_prvd_chainlink_pricefeed~move_file_to_ipfs(  )
    ENDIF.

  ENDMETHOD.


  METHOD zif_prvd_chainlink_pricefeed~generate_s4_market_rate_file.
    DATA:wa_pricefeed_results TYPE zprvd_pf_results,
         lv_pricefeed_json    TYPE string.
    DATA: lv_pricefeedresultfile TYPE string,
          lv_output_length       TYPE i,
          lt_binary_tab          TYPE TABLE OF bapiconten,
          lv_pf_filename         TYPE string.

    lv_pricefeed_json = /ui2/cl_json=>serialize( data = it_pricefeed_results pretty_name = /ui2/cl_json=>pretty_mode-low_case ).

    CONCATENATE 'LatestPriceFeed-' sy-datum '-' sy-timlo '.json' INTO lv_pf_filename.
    CONCATENATE iv_basepath lv_pf_filename  INTO lv_pricefeedresultfile.
    OPEN DATASET lv_pricefeedresultfile FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.

    TRANSFER lv_pricefeed_json TO lv_pricefeedresultfile.

    CLOSE DATASET lv_pricefeedresultfile.
    ev_filelocation = lv_pricefeedresultfile.
    ev_filename = lv_pf_filename.
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
    DATA: lt_pricefeed_results TYPE zif_prvd_chainlink_pricefeed=>tty_chainlink_pricefeed_result,
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
    DATA: ls_tcurr TYPE tcurr,
          lt_tcurr TYPE ftdf_tab_tcurr.

    LOOP AT it_pf_results ASSIGNING FIELD-SYMBOL(<fs_pf_result>).
      ls_tcurr-fcurr = <fs_pf_result>-fcurr.
      ls_tcurr-tcurr = <fs_pf_result>-tcurr.
      ls_tcurr-gdatu = <fs_pf_result>-gdatu.
      ls_tcurr-kurst = <fs_pf_result>-kurst.
      ls_tcurr-ukurs = <fs_pf_result>-formatted_amount.
      APPEND ls_tcurr TO lt_tcurr.
      CLEAR ls_tcurr.
    ENDLOOP.

    et_tcurr = lt_tcurr.

  ENDMETHOD.

  METHOD zif_prvd_chainlink_pricefeed~archive_files.
    "iv_directorylocation
    "iv_archivelocation
  ENDMETHOD.

  METHOD zif_prvd_chainlink_pricefeed~update_s4hana_market_rates.
    DATA: lt_updated_market_rates TYPE TABLE OF tcurr.

    lt_updated_market_rates = it_tcurr.
    CHECK lt_updated_market_rates IS NOT INITIAL.
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

  METHOD run_pricefeed_batch.

    DATA: lt_prvdpricefeed            TYPE STANDARD TABLE OF zprvdpricefeed,
          lv_sap_chainlink_timestampl TYPE timestampl,
          ls_execute_contract_resp    TYPE zif_proubc_nchain=>ty_executecontract_resp,
          ls_execute_contract_summary TYPE zif_proubc_nchain=>ty_executecontract_summary,
          ls_latestround              TYPE zif_prvd_chainlink_pricefeed=>ty_latestrounddata_result,
          ls_pricefeed_result         TYPE zif_prvd_chainlink_pricefeed=>ty_chainlink_pricefeed_result,
          lt_pricefeed_results        TYPE zif_prvd_chainlink_pricefeed=>tty_chainlink_pricefeed_result,
          lv_from_currency            TYPE waers_curc,
          lv_to_currency              TYPE waers_curc,
          lt_bpi_obj                  TYPE zif_prvd_chainlink_pricefeed=>tty_bpiobj.

    SELECT * FROM zprvdpricefeed INTO TABLE lt_prvdpricefeed
        WHERE zprvdnchainnetworkid = iv_networkid
        AND pairid IN it_selected_pfs.

    IF sy-subrc NE 0. "none of the selected pricefeeds found
    ENDIF.

    LOOP AT lt_prvdpricefeed ASSIGNING FIELD-SYMBOL(<fs_prvdpricefeed>).
      CLEAR: lv_from_currency, lv_to_currency.
      lv_from_currency = <fs_prvdpricefeed>-from_currency.
      lv_to_currency = <fs_prvdpricefeed>-to_currency.
      DATA: lv_pricepair         TYPE string,
            lv_pricefeed_resp_cd TYPE i.
      CONCATENATE lv_from_currency '/' lv_to_currency INTO lv_pricepair.

      me->zif_prvd_chainlink_pricefeed~execute_chainlink_pricefeed(
        EXPORTING
          iv_selected_pricefeed       = <fs_prvdpricefeed>
          IMPORTING
            es_execute_contract_resp    = ls_execute_contract_resp
            es_execute_contract_summary = ls_execute_contract_summary
            ev_httpresponsecode = lv_pricefeed_resp_cd
      ).
      IF ls_execute_contract_resp IS INITIAL.
        "message e000 zcl_prvdchainlinkmsg with lv_pricepair
      ENDIF.
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
          is_execute_contract_summary = ls_execute_contract_summary
          iv_from_currency = lv_from_currency
          iv_to_currency = lv_to_currency
        IMPORTING
          es_pricefeedresult = ls_pricefeed_result
      ).
      DATA: ls_pf_table_result TYPE zif_prvd_chainlink_pricefeed=>tty_pf_result,
            lt_tcurr           TYPE ftdf_tab_tcurr.
      me->save_result( EXPORTING is_pricefeedresult = ls_pricefeed_result
                       IMPORTING et_pf_table  = ls_pf_table_result ).
      me->zif_prvd_chainlink_pricefeed~format_to_market_rates( EXPORTING it_pf_results = ls_pf_table_result
                                                               IMPORTING et_tcurr = lt_tcurr ).
      me->zif_prvd_chainlink_pricefeed~update_s4hana_market_rates( EXPORTING it_tcurr = lt_tcurr ).
      IF lv_do_baseline IS NOT INITIAL.
        me->zif_prvd_chainlink_pricefeed~emit_baseline_zkp_msg(
          EXPORTING
            is_pricefeed_result =  ls_pf_table_result
          IMPORTING
            es_bpiobj           = lt_bpi_obj
        ).
        APPEND LINES OF LT_BPI_OBJ TO ET_ZKPS.
      ENDIF.
      APPEND ls_pricefeed_result TO lt_pricefeed_results.
    ENDLOOP.

    IF lv_do_ipfs IS NOT INITIAL.
      DATA: lv_basepath     TYPE zcasesensitivechar255,
            lv_filelocation TYPE string,
            lv_filename     TYPE string,
            lv_filenamestr  TYPE string,
            lv_movedfile    TYPE string,
            lv_ipfs_cid     TYPE string.

      lv_basepath = '/usr/sap/trans/data/'.

      me->zif_prvd_chainlink_pricefeed~generate_s4_market_rate_file(
        EXPORTING
          it_pricefeed_results = ls_pf_table_result
          iv_basepath          = lv_basepath
        IMPORTING
          ev_filelocation      = lv_filelocation
          ev_filename = lv_filename
      ).
*        lv_filenamestr = lv_filename.
*        CONCATENATE lv_basepath lv_filename INTO lv_movedfile SEPARATED BY '/'.
      me->zif_prvd_chainlink_pricefeed~move_file_to_ipfs(
        EXPORTING
          iv_filelocation = lv_filelocation
          iv_ipfsfilename = lv_filename
          IMPORTING
            ev_contentid    = lv_ipfs_cid
      ).

      ev_al11_file = lv_filelocation.
      ev_ipfs_cid = lv_ipfs_cid.
    ENDIF.

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
    lv_ipfsprojid = iv_ipfsp.
    lv_ipfsapikey = iv_ipfsk.
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
    IF sy-subrc NE 0.
      "problem with Nchain response
    ENDIF.
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

    "log latest round data
    ls_result-rawanswer = is_latestround-answer.
    ls_result-answeredinround = is_latestround-answeredinround.
    ls_result-roundid = is_latestround-roundid.
    ls_result-startedat = is_latestround-startedat.
    ls_result-updatedat = is_latestround-updatedat.

    "format to SAP standards
    me->format_eth_price(
      EXPORTING
        iv_chainlink_eth_price =  is_latestround-answer
        iv_to_currency         =  iv_to_currency
      IMPORTING
        ev_sap_eth_price       = ls_result-formatted_amount
    ).
    ls_result-from_currency = iv_from_currency.
    ls_result-to_currency = iv_to_currency.

    ls_result-exchange_type = 'M'.

    "add some other Nchain and SAP related info
    ls_result-user_responsible = sy-uname.
    ls_result-networkid =  is_execute_contract_summary-nchain_network_id.
    ls_result-walletid = is_execute_contract_summary-walletid.
    ls_result-smartcontractaddress = is_execute_contract_summary-smartcontract_addr.
    "pass result
    es_pricefeedresult = ls_result.
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
    lv_eth_formatted_price = lv_eth_floated_price.

    ev_sap_eth_price = lv_eth_formatted_price.
  ENDMETHOD.

  METHOD format_btc_price.
    DATA: lv_btc_formatted_price TYPE p DECIMALS 5, "9 character decimal type + up to 5 decimal places
          lv_btc_floated_price   TYPE float,
          lv_float_factor        TYPE float.

    CASE iv_to_currency.
      WHEN 'USD'.
        lv_float_factor = '0.00000001'.
      WHEN OTHERS.
        lv_float_factor = '0.00000001'.
    ENDCASE.

    lv_btc_floated_price = iv_chainlink_btc_price * lv_float_factor.
    lv_btc_formatted_price = lv_btc_floated_price.

    ev_sap_btc_price = lv_btc_formatted_price.
  ENDMETHOD.

  METHOD zif_prvd_chainlink_pricefeed~emit_baseline_zkp_msg.
    DATA: lv_setup_success     TYPE boolean,
          ls_protocol_msg_req  TYPE zif_proubc_baseline=>protocolmessage_req,
          ls_protocol_msg_resp TYPE zif_proubc_baseline=>protocolmessage_resp,
          lv_status            TYPE i,
          lv_apiresponse       TYPE REF TO data,
          lv_apiresponsestr    TYPE string,
          lt_updatedbpis       TYPE TABLE OF zbpiobj,
          lt_newbpis           TYPE TABLE OF zbpiobj,
          lt_final_updatedbpis TYPE TABLE OF zbpiobj,
          lt_final_newbpis     TYPE TABLE OF zbpiobj,
          ls_pf_result_data    TYPE REF TO data,
          lv_daily_pf_key      TYPE string,
          ls_baselineproto_msg TYPE zif_prvd_chainlink_pricefeed=>ty_baselined_result.

    lo_prvd_api_helper->setup_protocol_msg( IMPORTING setup_success = lv_setup_success ).

    LOOP AT is_pricefeed_result ASSIGNING FIELD-SYMBOL(<fs_pricefeed_result>).
      ls_baselineproto_msg-from_currency = <fs_pricefeed_result>-fcurr.
      ls_baselineproto_msg-to_currency = <fs_pricefeed_result>-tcurr.
      ls_baselineproto_msg-formatted_amount = <fs_pricefeed_result>-formatted_amount.
      ls_baselineproto_msg-exchange_type = <fs_pricefeed_result>-kurst.
      ls_baselineproto_msg-rawanswer = <fs_pricefeed_result>-rawanswer.
      ls_baselineproto_msg-answeredinround = <fs_pricefeed_result>-answeredinround.
      ls_baselineproto_msg-roundid = <fs_pricefeed_result>-roundid.
      ls_baselineproto_msg-smartcontractaddress = <fs_pricefeed_result>-smartcontractaddress.
      ls_baselineproto_msg-networkid = <fs_pricefeed_result>-networkid.
      CONCATENATE <fs_pricefeed_result>-kurst
                  <fs_pricefeed_result>-fcurr
                  <fs_pricefeed_result>-tcurr
                 <fs_pricefeed_result>-gdatu
      INTO lv_daily_pf_key SEPARATED BY '|'.
      ls_baselineproto_msg-dailypfkey = lv_daily_pf_key.
      GET REFERENCE OF ls_baselineproto_msg INTO ls_pf_result_data.

      "request to /api/v1/protocol_messages
      ls_protocol_msg_req-payload = ls_pf_result_data.
      ls_protocol_msg_req-payload_mimetype = 'json'.
      ls_protocol_msg_req-type = 'PRVDChainlinkPriceFeedSync'.


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
        CLEAR: wa_bpiobj.
      ELSE. "log error message
      ENDIF.

    ENDLOOP.

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
    DATA: lv_filecontent_xstr TYPE xstring,
          lv_filecontent_str  TYPE string,
          lv_xcontentlength   TYPE i.
    zcl_proubc_file_helper=>open_file_generic(
      EXPORTING
        iv_file_location = iv_filelocation
      IMPORTING
        ev_filecontent_x = lv_filecontent_xstr
        ev_filecontent   = lv_filecontent_str
        ev_length = lv_xcontentlength
    ).
    zcl_proubc_file_helper=>transfer_file_to_ipfs(
      EXPORTING
        iv_filecontent_x =  lv_filecontent_xstr
        iv_filecontent = lv_filecontent_str
        iv_filename = iv_ipfsfilename
        iv_filetype = 'json'
        iv_ipfsprojid    = lv_ipfsprojid
        iv_ipfsapikey = lv_ipfsapikey
        iv_xcontentlength = lv_xcontentlength
     IMPORTING
       ev_contentid     = ev_contentid
    ).
  ENDMETHOD.

  METHOD save_result.
    DATA: ls_prvd_pf_results TYPE zprvd_pf_results,
          lt_prvd_pf_results TYPE TABLE OF zprvd_pf_results.

    "map to to db structure
    ls_prvd_pf_results-fcurr = is_pricefeedresult-from_currency.
    ls_prvd_pf_results-gdatu = sy-datum. " todo add to is_pricefeedresult
    ls_prvd_pf_results-kurst = is_pricefeedresult-exchange_type.
    ls_prvd_pf_results-networkid = is_pricefeedresult-networkid.
    ls_prvd_pf_results-smartcontractaddress = is_pricefeedresult-smartcontractaddress.
    ls_prvd_pf_results-tcurr = is_pricefeedresult-to_currency.
    ls_prvd_pf_results-txn_hash = is_pricefeedresult-txn_hash.
    ls_prvd_pf_results-txn_processed_at = is_pricefeedresult-txn_processed_at.
    ls_prvd_pf_results-user_responsible = is_pricefeedresult-user_responsible.
    ls_prvd_pf_results-walletid = is_pricefeedresult-walletid.
    ls_prvd_pf_results-answeredinround = is_pricefeedresult-answeredinround.
    ls_prvd_pf_results-roundid = is_pricefeedresult-roundid.
    ls_prvd_pf_results-rawanswer = is_pricefeedresult-rawanswer.
    ls_prvd_pf_results-formatted_amount = is_pricefeedresult-formatted_amount.


    APPEND ls_prvd_pf_results TO lt_prvd_pf_results.
    et_pf_table = lt_prvd_pf_results.
    CLEAR ls_prvd_pf_results.
    MODIFY zprvd_pf_results FROM TABLE lt_prvd_pf_results.

  ENDMETHOD.

  METHOD zif_prvd_chainlink_pricefeed~execute_chainlink_pricefeed.
    DATA: ls_pricefeedwallet            TYPE zif_proubc_nchain=>ty_createhdwalletrequest,
          lv_getwallet_str              TYPE string,
          lv_getwallet_data             TYPE REF TO data,
          lv_getwallet_responsecode     TYPE i,
          ls_wallet_created             TYPE zif_proubc_nchain=>ty_hdwalletcreate_resp,
          ls_selectedcontract           TYPE zif_proubc_nchain=>ty_chainlinkpricefeed_req,
          lv_createdcontract_str        TYPE string,
          lv_createdcontract_data       TYPE REF TO data,
          lv_createdcontract_responsecd TYPE i,
          ls_executecontract            TYPE zif_proubc_nchain=>ty_executecontractrequest,
          lv_executecontract_str        TYPE string,
          lv_executecontract_xstr       TYPE xstring,
          lv_executecontract_data       TYPE REF TO data,
          lv_executecontract_responsecd TYPE i,
          lv_network_contract_id        TYPE zproubc_smartcontract_addr,
          lv_prvd_stack_contract_id     TYPE zcasesensitive_str,
          ls_execute_contract_resp      TYPE zif_proubc_nchain=>ty_executecontract_resp,
          ls_execute_contract_summary   TYPE zif_proubc_nchain=>ty_executecontract_summary.

    ls_pricefeedwallet-purpose = 44.
    lo_prvd_nchain_helper->get_nchain_client( )->zif_proubc_nchain~createhdwallet( EXPORTING is_walletrequest = ls_pricefeedwallet
                                                         IMPORTING ev_apiresponsestr   = lv_getwallet_str
                                                                   ev_apiresponse       = lv_getwallet_data
                                                                   ev_httpresponsecode = lv_getwallet_responsecode ).
    CASE lv_getwallet_responsecode.
      WHEN 201.
        /ui2/cl_json=>deserialize( EXPORTING json = lv_getwallet_str CHANGING data = ls_wallet_created ).
      WHEN OTHERS. "add error handling
        ev_httpresponsecode = lv_getwallet_responsecode.
        RETURN.
    ENDCASE.
    CONCATENATE iv_selected_pricefeed-from_currency iv_selected_pricefeed-to_currency INTO DATA(lv_pfname) SEPARATED BY '/'.
    lo_prvd_nchain_helper->smartcontract_factory(  EXPORTING iv_smartcontractaddress = iv_selected_pricefeed-zprvdsmartcontractaddr
                                          iv_name                 = lv_pfname
                                          iv_walletaddress        = ls_wallet_created-id  "from the wallet we created earlier
                                          iv_nchain_networkid     = iv_selected_pricefeed-zprvdnchainnetworkid " polygon mumbai testnet nchain id
                                          iv_contracttype         = 'price-feed'
                                IMPORTING es_selectedcontract = ls_selectedcontract ).
    lo_prvd_nchain_helper->get_nchain_client( )->zif_proubc_nchain~createpricefeedcontract(
      EXPORTING
        iv_smartcontractaddr = iv_selected_pricefeed-zprvdsmartcontractaddr
        is_pricefeedcontract = ls_selectedcontract
      IMPORTING
        ev_apiresponsestr    = lv_createdcontract_str
        ev_apiresponse       = lv_createdcontract_data
        ev_httpresponsecode  = lv_createdcontract_responsecd
    ).
    CASE lv_createdcontract_responsecd.
      WHEN 201.

        FIELD-SYMBOLS: <fs_prvd_stack_contractid>     TYPE any,
                       <fs_prvd_stack_contractid_str> TYPE string.

        IF lv_createdcontract_data IS NOT INITIAL.
          ASSIGN lv_createdcontract_data->* TO FIELD-SYMBOL(<ls_contractdata>).
          ASSIGN COMPONENT 'ID' OF STRUCTURE <ls_contractdata> TO <fs_prvd_stack_contractid>.
          ASSIGN <fs_prvd_stack_contractid>->* TO <fs_prvd_stack_contractid_str>.
          lv_prvd_stack_contract_id = <fs_prvd_stack_contractid_str>.
        ENDIF.
        ls_executecontract-method = 'latestRoundData'.
        ls_executecontract-value = 0.
        ls_executecontract-wallet_id = ls_wallet_created-id.
      WHEN 404. "contract not found - might not be deployed
        ev_httpresponsecode = lv_createdcontract_responsecd.
        RETURN.
      WHEN OTHERS.
        ev_httpresponsecode = lv_createdcontract_responsecd.
        RETURN.
    ENDCASE.
    lo_prvd_nchain_helper->get_nchain_client( )->zif_proubc_nchain~executecontract(
       EXPORTING
         iv_contract_id      = lv_prvd_stack_contract_id
         is_execcontractreq  = ls_executecontract
       IMPORTING
         ev_apiresponsestr   = lv_executecontract_str
         ev_apiresponsexstr  = lv_executecontract_xstr
         ev_apiresponse      =  lv_executecontract_data
         ev_httpresponsecode =  lv_executecontract_responsecd
     ).
    CASE lv_executecontract_responsecd.
      WHEN 200.
        ls_execute_contract_summary-nchain_network_id = iv_selected_pricefeed-zprvdnchainnetworkid.
        ls_execute_contract_summary-prvd_stack_contractid = lv_prvd_stack_contract_id.
        ls_execute_contract_summary-smartcontract_addr = iv_selected_pricefeed-zprvdsmartcontractaddr.
        ls_execute_contract_summary-walletid = ls_wallet_created-id.
        "TODO - losing response values when deserializing. Round IDs surpass p8 type
        /ui2/cl_json=>deserialize( EXPORTING jsonx = lv_executecontract_xstr CHANGING data = ls_execute_contract_resp  ).
        ASSIGN lv_executecontract_data->* TO FIELD-SYMBOL(<ls_contractoutputs>).
        ASSIGN COMPONENT 'RESPONSE' OF STRUCTURE <ls_contractoutputs> TO FIELD-SYMBOL(<fs_executecontract_resp>).
        es_execute_contract_resp = ls_execute_contract_resp.
        es_execute_contract_summary = ls_execute_contract_summary.

      WHEN OTHERS.
        ev_httpresponsecode = lv_executecontract_responsecd.
        RETURN.
    ENDCASE.
  ENDMETHOD.

ENDCLASS.
