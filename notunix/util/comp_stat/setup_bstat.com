$!========================================================================
$!
$! Name      : SETUP_BSTAT
$!
$! Purpose   : Define symbol for batch status utility
$!
$! Arguments : none
$!
$! Created  12-JUL-1993   Michael Diesburg
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$!
$  BST*AT   :==     $D0$UTIL:BSTAT
$!
$EXIT:
$   EXIT
