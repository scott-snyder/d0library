$!========================================================================
$!
$! Name      : SETUP_L2_CODE_FRAGMENTS
$!
$! Purpose   : define logicals for useful code fragments to get with edfor
$!
$! Arguments : 
$!
$! Created  16-OCT-1991   James T. Linnemann
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$ DEFINE FIRST D0$LEVEL2$SOURCE:FIRST.FORT
$ DEFINE CURRENT D0$LEVEL2$SOURCE:CURRENT.FORT
$ DEFINE EXTERNAL D0$LEVEL2$SOURCE:EXTERNAL.FORT
$ DEFINE PICK D0$LEVEL2$SOURCE:PICK.FORT
$ DEFINE PATH D0$LEVEL2$SOURCE:PATH.FORT
$EXIT:
$   EXIT
