$!========================================================================
$!
$! Name      : SUB_MERGE
$!
$! Purpose   : MERGE FILES
$!
$! Arguments : 
$!
$! Created  13-SEP-1990   Rajendran Raja
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$ P1 = -
"SORT_BLND16_UNSORT.DAT01,SORT_BLND16_UNSORT.DAT02,SORT_BLND16_UNSORT.DAT03,"+-
"SORT_BLND16_UNSORT.DAT04,SORT_BLND16_UNSORT.DAT05,SORT_BLND16_UNSORT.DAT06,"+-
"SORT_BLND16_UNSORT.DAT07,SORT_BLND16_UNSORT.DAT08" 
$ P2 = "MERGE_BLND16_1TO8.DAT"
$@BETA:MERGE 'P1' 'P2'
$EXIT:
$   EXIT
