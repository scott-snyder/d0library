$!========================================================================
$!
$! Name      : DEFINE_DBL3
$!
$! Purpose   : Define old and new format dbl3 logicals
$!
$! Arguments : P1   OLD     To access old database
$!                  NEW     To access new database
$!
$! Created   7-AUG-1992   SHAHRIAR ABACHI
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT


$ IF P1 .EQS. "" THEN -
   INQUIRE P1 " Which node (D0FS, D0, ...) ?"
$ IF (P1 .EQS. "")
$ THEN
$  write sys$output -
     " Database node location logical ''P1' is undefined"
$  GOTO EXIT
$ ENDIF
$!
$ define dbl3$cal "''p1'::dbl3$cal:"
$ define dbl3$muo "''p1'::dbl3$muo:"
$ define dbl3$cdc "''p1'::dbl3$cdc:"
$ define dbl3$fdc "''p1'::dbl3$fdc:"
$ define dbl3$vtx "''p1'::dbl3$vtx:"
$ define dbl3$trd "''p1'::dbl3$trd:"
$ define dbl3$dbm "''p1'::dbl3$dbm:"
$ define dbl3$hvm "''p1'::dbl3$hvm:"
$ define dbl3$lum "''p1'::dbl3$lum:"
$!
$EXIT:
$   EXIT
