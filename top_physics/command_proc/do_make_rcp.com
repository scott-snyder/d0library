$!========================================================================
$!
$! Name      : DO_MAKE_RCP
$!
$! Purpose   : Make all needed monte rcp files
$!
$! Arguments : 
$!
$! Created  20-SEP-1993   Rajendran Raja
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$@make_rcp 100 250 140
$@make_rcp 80 230  120
$@make_rcp 80 230  100
$EXIT:
$   EXIT
