$!========================================================================
$!
$! Name      : UPDATE_L2_LIB
$!
$! Purpose   : update a l2 prod library
$!
$! Arguments : none
$!
$! Created  20-AUG-1992   James T. Linnemann
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$ @l2$prod:l2prod_setup
$ fake_gamma keep
$ @L2$PROD:mark_l2prod_lib
$EXIT:
$   EXIT
