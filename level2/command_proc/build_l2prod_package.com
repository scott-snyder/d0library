$!========================================================================
$!
$! Name      : BUILD_L2PROD_PACKAGE
$!
$! Purpose   : Build the L2PROD Package
$!
$! Arguments : 
$!
$! Created   1-FEB-1993   Alan M. Jonckheere
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO ERROR_EXIT
$   ON CONTROL_Y THEN $ GOTO CONTY_EXIT
$
$
$NORM_EXIT:
$   d0$status :== TRUE
$   EXIT
$CONTY_EXIT:
$   WRITE SYS$OUTPUT " ^Y Exit"
$ERROR_EXIT:
$   d0$status :== FALSE
$   EXIT
