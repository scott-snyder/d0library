$!========================================================================
$!
$! Name      : ANALYZE_RMS
$!
$! Purpose   : ANALYZES FILE AND PRODUCES .FDL
$!
$! Arguments : 
$!
$! Created  26-SEP-1990   Rajendran Raja
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$ SET DEF SF13:
$ IF P1 .EQS. "" THEN P1 = "BLND15_BLND16_SHOWERLIBRARY.DAT"
$! IF P2 .EQS. "" THEN P2 = "ANAL_KEYED"
$ANALYZE/RMS_FILE/FDL/OUT='P2'.FDL 'P1'
$EXIT:
$   EXIT
