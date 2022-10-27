*&---------------------------------------------------------------------*
*& Report zprvd_chainlink_pricefeed
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zprvd_chainlink_pricefeed.

DATA: lo_prvd_chainlink_pricefeed TYPE REF TO zcl_prvd_chainlink_pricefeed,
      lt_pricefeed_messages       TYPE TABLE OF bapiret2.

"Todo add selection range
"select-OPTIONS: s_pricefeeds for zprvdpricefeed-
PARAMETERS: p_tenant TYPE zprvdtenantid,
            p_sbjact TYPE zprvdtenantid,
            p_wrkgrp TYPE zprvdtenantid.

INITIALIZATION.
  GET PARAMETER ID 'ZPRVDTENANT' FIELD p_tenant.
  GET PARAMETER ID 'ZPRVDSUBJACCTID' FIELD p_sbjact.
  GET PARAMETER ID 'ZPRVDWRKGRPID' FIELD p_wrkgrp.

START-OF-SELECTION.
  lo_prvd_chainlink_pricefeed = NEW zcl_prvd_chainlink_pricefeed( ).
  lo_prvd_chainlink_pricefeed->setup( EXPORTING iv_tenant = p_tenant
                                                iv_subj_acct = p_sbjact
                                                iv_workgroup_id = p_wrkgrp ).
  lo_prvd_chainlink_pricefeed->zif_prvd_chainlink_pricefeed~prvd_authenticate( iv_authtype = 'R'  ).

  lo_prvd_chainlink_pricefeed->zif_prvd_chainlink_pricefeed~call_chainlink_pricefeeds( ).

  LOOP AT lt_pricefeed_messages ASSIGNING FIELD-SYMBOL(<fs_pricefeed_messages>).
  ENDLOOP.
