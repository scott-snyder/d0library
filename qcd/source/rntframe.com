$!========================================================================
$!
$! Name      : RNTFRAME
$!
$! Purpose   : RUN NTFRAME.FOR
$!
$! Arguments : NONE
$!
$! Created   2-SEP-1993   Freedy Nang
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$ DEFINE D0$PHYSICS_UTIL D0$BETA:[PHYSICS_UTIL], -
                         D0$PHYSICS_UTIL$ROOT:[000000], -
$ DEFINE QCD_JET_CORRECTION_RCP D0$PHYSICS_UTIL:QCD_JET_CORRECTION.RCP
$ DEFINE NTUP_AREA       PRJ$ROOT248:[QCD_6.IADATA]
$ DEFINE NTUP_RCP        D0$QCD$ROOT:[RCP]ANALYSIS.RCP
$ R/NODEBUG  NTFRAME
$EXIT:
$   EXIT
