$!========================================================================
$!
$! Name      : DEFINE_L2_PASS
$!
$! Purpose   : define the production version and pass number for level 2
$!              version is only for large changes (eg include files)
$!              pass is for small changes (one or a few routines)
$!                      or anything else which provokes a relink
$!
$! Arguments : none
$!
$! Created  20-MAY-1992   James T. Linnemann (should be written by release)
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$ L2_VERSION_NUM == 7
$ L2_PASS_NUM == 0
$EXIT:
$   EXIT
