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
   INQUIRE P1 " Which databases (OLD or NEW) ?"
$ IF (P1 .NES. "OLD") .AND. (P1 .NES. "NEW")
$ THEN
$  write sys$output -
     " Database location logical ''P1' is undefined"
$  GOTO EXIT
$ ENDIF
$!
$if p1 .eqs. "OLD" 
$ THEN
$ define dbl3$cal d0fsa::dbl3_old$cal:
$ define dbl3$muo d0fsa::dbl3_old$muo:
$ define dbl3$cdc d0fsa::dbl3_old$cdc:
$ define dbl3$fdc d0fsa::dbl3_old$fdc:
$ define dbl3$vtx d0fsa::dbl3_old$vtx:
$ define dbl3$trd d0fsa::dbl3_old$trd:
$ define dbl3$dbm d0fsa::dbl3_old$dbm:
$ define dbl3$hvm d0fsa::dbl3_old$hvm:
$ define dbl3$lum d0fsa::dbl3_old$lum:
$endif
$!
$if p1 .eqs. "NEW" 
$ THEN
$ define dbl3$cal d0fsa::dbl3$cal:
$ define dbl3$muo d0fsa::dbl3$muo:
$ define dbl3$cdc d0fsa::dbl3$cdc:
$ define dbl3$fdc d0fsa::dbl3$fdc:
$ define dbl3$vtx d0fsa::dbl3$vtx:
$ define dbl3$trd d0fsa::dbl3$trd:
$ define dbl3$dbm d0fsa::dbl3$dbm:
$ define dbl3$hvm d0fsa::dbl3$hvm:
$ define dbl3$lum d0fsa::dbl3$lum:
$endif
$!
$EXIT:
$   EXIT
