$!========================================================================
$!
$! Name      : DEFINE_MLT
$!
$! Purpose   : Define MLT$INCLUDE as a search list for UFFARM
$!
$! Arguments : 
$!
$! Created  30-MAY-1990   A.M.Jonckheere
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$!
$   mlt = f$trnlnm("MLT$INCLUDE")
$   IF mlt .eqs. ""
$   THEN
$       DEFINE MLT$INCLUDE D0$INC
$       WRITE SYS$OUTPUT "MLT$INCLUDE defined"
$   ELSE
$       WRITE SYS$OUTPUT "MLT$INCLUDE already defined"
$   ENDIF
$!
$EXIT:
$   EXIT
