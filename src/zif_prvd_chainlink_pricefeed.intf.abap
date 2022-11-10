INTERFACE zif_prvd_chainlink_pricefeed
  PUBLIC .

  TYPES: BEGIN OF ty_chainlink_pricefeed_result,
           from_currency        TYPE waers_curc,
           to_currency          TYPE waers_curc,
           exchange_type        TYPE kurst_curr,
           formatted_amount     TYPE UKURS_CURR,
           smartcontractaddress TYPE zproubc_smartcontract_addr,
           prvdstackcontractid  TYPE zcasesensitive_str,
           networkid            TYPE zprvd_nchain_networkid,
           txn_hash             TYPE string,
           txn_processed_at     TYPE timestampl,
           user_responsible     TYPE sy-uname,
           walletid             TYPE zproubc_smartcontract_addr,
           answeredinround      TYPE char25,
           roundid              TYPE char25,
           rawanswer            TYPE char80,
           startedat            TYPE char80,
           updatedat            TYPE char80,
         END OF ty_chainlink_pricefeed_result.
  TYPES: tty_pricefeed_results TYPE STANDARD TABLE OF ty_chainlink_pricefeed_result.

  TYPES: BEGIN OF ty_latestrounddata_result,
           roundid         TYPE int8, "uint80
           answer          TYPE int8, "int256 <-- this is your price, still needs some unit conversions for SAP
           startedat       TYPE int8, "uint256
           updatedat       TYPE int8, "uint256
           answeredinround TYPE int8, "uint80
         END OF ty_latestrounddata_result.

  TYPES: BEGIN OF ty_baselined_result,
           networkid            TYPE zprvd_nchain_networkid,
           smartcontractaddress TYPE zproubc_smartcontract_addr,
           from_currency        TYPE waers_curc,
           to_currency          TYPE waers_curc,
           exchange_type    TYPE kurst_curr,
           rawanswer            TYPE char80,
           formatted_amount     TYPE ukurs,
           answeredinround      TYPE char25,
           roundid              TYPE char25,
         END OF ty_baselined_result.

  types: tty_pf_result type standard table of zprvd_pf_results,
         tty_tcurr type STANDARD TABLE OF tcurr,
         tty_bpiobj type STANDARD TABLE OF zbpiobj.

  METHODS:  prvd_authenticate IMPORTING iv_authtype   TYPE char1
                                        iv_prvduser   TYPE string OPTIONAL
                                        iv_prvduserpw TYPE string OPTIONAL,
    call_chainlink_pricefeeds,
    execute_chainlink_pricefeed IMPORTING iv_selected_pricefeed       TYPE zprvdpricefeed
                                EXPORTING es_execute_contract_resp    TYPE zif_proubc_nchain=>ty_executecontract_resp
                                          es_execute_contract_summary TYPE zif_proubc_nchain=>ty_executecontract_summary,

    generate_s4_market_rate_file IMPORTING it_pricefeed_results TYPE tty_pricefeed_results
                                 EXPORTING ev_filelocation      TYPE zcasesensitivechar255,
    move_file_to_ipfs     IMPORTING iv_filelocation TYPE zcasesensitivechar255
                          EXPORTING ev_contentid    TYPE zcasesensitivechar255,
    read_market_rate_file IMPORTING iv_directorylocation TYPE zcasesensitivechar255
                          EXPORTING et_tcurr             TYPE ftdf_tab_tcurr,
    format_to_market_rates IMPORTING it_pf_results TYPE tty_pf_result
                           EXPORTING et_tcurr      TYPE ftdf_tab_tcurr,
    archive_files IMPORTING iv_directorylocation TYPE zcasesensitivechar255
                            iv_archivelocation   TYPE zcasesensitivechar255,
    update_s4hana_market_rates IMPORTING it_tcurr TYPE ftdf_tab_tcurr,
    emit_baseline_zkp_msg IMPORTING is_pricefeed_result TYPE tty_pf_result
                          EXPORTING es_bpiobj           TYPE tty_bpiobj.

ENDINTERFACE.
