INTERFACE zif_prvd_chainlink_pricefeed
  PUBLIC .

  TYPES: BEGIN OF ty_chainlink_pricefeed_result,
           from_currency        TYPE waers_curc,
           to_currency          TYPE waers_curc,
           exchange_type        TYPE kurst_curr,
           formatted_amount     TYPE ukurs_curr,
           smartcontractaddress TYPE zprvd_smartcontract_addr,
           prvdstackcontractid  TYPE zcasesensitive_str,
           networkid            TYPE zprvd_nchain_networkid,
           txn_hash             TYPE string,
           txn_processed_at     TYPE timestampl,
           user_responsible     TYPE sy-uname,
           walletid             TYPE zprvd_smartcontract_addr,
           answeredinround      TYPE char25,
           roundid              TYPE char25,
           rawanswer            TYPE char80,
           startedat            TYPE char80,
           updatedat            TYPE char80,
         END OF ty_chainlink_pricefeed_result.
  TYPES: tty_chainlink_pricefeed_result TYPE STANDARD TABLE OF ty_chainlink_pricefeed_result.

  TYPES: BEGIN OF ty_latestrounddata_result,
           roundid         TYPE int8,
           answer          TYPE int8,
           startedat       TYPE int8,
           updatedat       TYPE int8,
           answeredinround TYPE int8,
         END OF ty_latestrounddata_result.

  TYPES: BEGIN OF ty_baselined_result,
           networkid            TYPE zprvd_nchain_networkid,
           smartcontractaddress TYPE zprvd_smartcontract_addr,
           from_currency        TYPE waers_curc,
           to_currency          TYPE waers_curc,
           exchange_type        TYPE kurst_curr,
           rawanswer            TYPE char80,
           formatted_amount     TYPE ukurs,
           answeredinround      TYPE char25,
           roundid              TYPE char25,
           dailypfkey           TYPE string,
         END OF ty_baselined_result.

  TYPES: tty_pf_result TYPE STANDARD TABLE OF zprvd_pf_results,
         tty_tcurr     TYPE STANDARD TABLE OF tcurr,
         tty_bpiobj    TYPE STANDARD TABLE OF zbpiobj.

  METHODS:
    "! Authenticates the SAP user to their PRVD account via
    prvd_authenticate IMPORTING iv_authtype   TYPE char1
                                iv_prvduser   TYPE string OPTIONAL
                                iv_prvduserpw TYPE string OPTIONAL,
    "! Uses the built-in Chainlink price feed example with ETH/USD on Polygon Mumbai
    call_chainlink_pricefeeds,
    "! Executes the selected Chainlink price feed
    execute_chainlink_pricefeed IMPORTING iv_selected_pricefeed       TYPE zprvdpricefeed
                                EXPORTING es_execute_contract_resp    TYPE zif_prvd_nchain=>ty_executecontract_resp
                                          es_execute_contract_summary TYPE zif_prvd_nchain=>ty_executecontract_summary
                                          ev_httpresponsecode TYPE i,
    "! Generates a file representing the price feed results
    generate_s4_market_rate_file IMPORTING it_pricefeed_results TYPE tty_pf_result
                                           iv_basepath          TYPE zcasesensitivechar255
                                 EXPORTING ev_filelocation      TYPE string
                                           ev_filename          TYPE string,
    "! Moves the price feed results file to IPFS
    move_file_to_ipfs     IMPORTING iv_filelocation TYPE string
                                    iv_ipfsfilename type string
                          EXPORTING ev_contentid    TYPE string,
    "! Loads the data from a file into a TCURR format
    read_market_rate_file IMPORTING iv_directorylocation TYPE zcasesensitivechar255
                          EXPORTING et_tcurr             TYPE ftdf_tab_tcurr,
    "! Formats the price feed results table data to TCURR structure format
    format_to_market_rates IMPORTING it_pf_results TYPE tty_pf_result
                           EXPORTING et_tcurr      TYPE ftdf_tab_tcurr,
    "! Archives the given set of Pricefeed result files
    archive_files IMPORTING iv_directorylocation TYPE zcasesensitivechar255
                            iv_archivelocation   TYPE zcasesensitivechar255,
    "! Updates the currency price data in TCURR
    update_s4hana_market_rates IMPORTING it_tcurr TYPE ftdf_tab_tcurr,
    "! Creates a PRVD Baseline zk proof of the price feed / TCURR data used
    emit_baseline_zkp_msg IMPORTING is_pricefeed_result TYPE tty_pf_result
                          EXPORTING es_bpiobj           TYPE tty_bpiobj.

ENDINTERFACE.
