$!========================================================================
$!
$! Name      : SETUP_PAW
$!
$! Purpose   : 
$!
$! Arguments : 
$!
$! Created   3-APR-1992   Stan M. Krzywdzinski
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$   PAW :== $D0$CERNLIB:PAW_DI3000
$   SHOW SYM PAW
$EXIT:
$   EXIT
