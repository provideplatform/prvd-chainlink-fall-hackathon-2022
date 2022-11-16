*&---------------------------------------------------------------------*
*& Report zprvd_chainlink_pricefeed
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zprvd_chainlink_pricefeed.

DATA: lo_prvd_chainlink_pricefeed TYPE REF TO zcl_prvd_chainlink_pricefeed,
      lt_pricefeed_messages       TYPE TABLE OF bapiret2,
      lv_pfids                    TYPE zprvdpricefeed-pairid,
      lt_pf_pairids_rt            type zproubc_pf_pairid_rt.

SELECT-OPTIONS: s_pfids FOR lv_pfids.
PARAMETERS: p_netwrk TYPE zprvd_nchain_networkid,
            p_tenant TYPE zprvdtenantid,
            p_sbjact TYPE zprvdtenantid,
            p_wrkgrp TYPE zprvdtenantid,
            p_zkp    TYPE char1 AS CHECKBOX,
            p_ipfs   TYPE char1 AS CHECKBOX,
            p_ipfsp  type zcasesensitive_str,
            p_ipfsk  type zcasesensitive_str.

INITIALIZATION.
  GET PARAMETER ID 'ZPRVDTENANT' FIELD p_tenant.
  GET PARAMETER ID 'ZPRVDSUBJACCTID' FIELD p_sbjact.
  GET PARAMETER ID 'ZPRVDWRKGRPID' FIELD p_wrkgrp.

START-OF-SELECTION.
  MOVE-CORRESPONDING s_pfids[] to lt_pf_pairids_rt[].
  lo_prvd_chainlink_pricefeed = NEW zcl_prvd_chainlink_pricefeed( ).
  lo_prvd_chainlink_pricefeed->setup( EXPORTING iv_tenant = p_tenant
                                                iv_subj_acct = p_sbjact
                                                iv_workgroup_id = p_wrkgrp
                                                iv_do_baseline = p_zkp
                                                iv_do_ipfs = p_ipfs
                                                iv_ipfsp = p_ipfsp
                                                iv_ipfsk = p_ipfsk ).
  lo_prvd_chainlink_pricefeed->zif_prvd_chainlink_pricefeed~prvd_authenticate( iv_authtype = 'R'  ).

  "does the existing ETH/USD price feed from polygon mumbai with hardcoded values
  "lo_prvd_chainlink_pricefeed->zif_prvd_chainlink_pricefeed~call_chainlink_pricefeeds( ).
  lo_prvd_chainlink_pricefeed->run_pricefeed_batch(
    EXPORTING
      iv_networkid    = p_netwrk
      it_selected_pfs = lt_pf_pairids_rt
  ).

  LOOP AT lt_pricefeed_messages ASSIGNING FIELD-SYMBOL(<fs_pricefeed_messages>).
  ENDLOOP.

  IF p_zkp = 'X'.
    NEW-LINE.
    WRITE: 'Created PRVD Baseline ZKP for the price feed result use'.
    "todo list the ZKPs here
  ENDIF.

  IF p_ipfs = 'X'.
    NEW-LINE.
    WRITE: 'Shared price feed results to IPFS!'.
    "todo list content ids / file ids
  ENDIF.
