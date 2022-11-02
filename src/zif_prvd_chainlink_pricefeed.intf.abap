INTERFACE zif_prvd_chainlink_pricefeed
  PUBLIC .

  TYPES: BEGIN OF ty_chainlink_pricefeed_result,
           from_currency        TYPE waers,
           to_currency          TYPE waers,
           smartcontractaddress TYPE zproubc_smartcontract_addr,
           prvdstackcontractid type zcasesensitive_str,
           networkid            TYPE zprvd_nchain_networkid,
           txn_hash             TYPE string,
           txn_processed_at     TYPE timestampl,
           user_responsible     TYPE sy-uname,
           walletid             TYPE zproubc_smartcontract_addr,
         END OF ty_chainlink_pricefeed_result.
  TYPES: tty_pricefeed_results TYPE STANDARD TABLE OF ty_chainlink_pricefeed_result.

  TYPES: BEGIN OF ty_latestrounddata_result,
           roundid         TYPE int8, "uint80
           answer          TYPE int8, "int256 <-- this is your price, still needs some unit conversions for SAP
           startedat       TYPE int8, "uint256
           updatedat       TYPE int8, "uint256
           answeredinround TYPE int8, "uint80
         END OF ty_latestrounddata_result.

  METHODS:  prvd_authenticate IMPORTING iv_authtype   TYPE char1
                                        iv_prvduser   TYPE string OPTIONAL
                                        iv_prvduserpw TYPE string OPTIONAL,
    call_chainlink_pricefeeds,
    generate_s4_market_rate_file IMPORTING it_pricefeed_results TYPE tty_pricefeed_results
                                 EXPORTING ev_filelocation      TYPE zcasesensitivechar255,
    read_market_rate_file IMPORTING iv_directorylocation TYPE zcasesensitivechar255
                          EXPORTING et_tcurr             TYPE ftdf_tab_tcurr,
    archive_files IMPORTING iv_directorylocation TYPE zcasesensitivechar255
                            iv_archivelocation   TYPE zcasesensitivechar255,
    update_s4hana_market_rates IMPORTING it_tcurr TYPE ftdf_tab_tcurr.

ENDINTERFACE.
