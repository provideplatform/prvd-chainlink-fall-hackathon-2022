*&---------------------------------------------------------------------*
*& Report zprvd_chainlink_pricefeed
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zprvd_chainlink_pricefeed.

DATA: lo_prvd_chainlink_pricefeed TYPE REF TO zcl_prvd_chainlink_pricefeed.

PARAMETERS:   p_tenant TYPE zprvdtenantid,
              p_sbjact TYPE zprvdtenantid,
              p_wrkgrp TYPE zprvdtenantid.

INITIALIZATION.
GET PARAMETER ID 'ZPRVDTENANT' FIELD p_tenant.
GET PARAMETER ID 'ZPRVDSUBJACCTID' FIELD p_sbjact.
GET PARAMETER ID 'ZPRVDWRKGRPID' FIELD p_wrkgrp.

START-OF-SELECTION.
lo_prvd_chainlink_pricefeed = NEW zcl_prvd_chainlink_pricefeed( ).
lo_prvd_chainlink_pricefeed->zif_prvd_chainlink_pricefeed~prvd_authenticate( iv_authtype = 'R'  ).
