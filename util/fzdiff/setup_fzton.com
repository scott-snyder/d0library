$!========================================================================
$!
$! Name      : SETUP_FZTON
$!
$! Purpose   : Setup FZTON
$!
$! Arguments : None
$!
$! Created  11-SEP-1992   Herbert Greenlee
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
 FZTON :== $D0$UTIL:FZTON
$EXIT:
$   EXIT
