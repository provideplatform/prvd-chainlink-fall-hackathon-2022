*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZPRVDPRICEFEED..................................*
DATA:  BEGIN OF STATUS_ZPRVDPRICEFEED                .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZPRVDPRICEFEED                .
CONTROLS: TCTRL_ZPRVDPRICEFEED
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZPRVDPRICEFEED                .
TABLES: ZPRVDPRICEFEED                 .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
