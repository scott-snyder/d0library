$!========================================================================
$!
$! Name      : DEALLOCATE_TAPE
$!
$! Purpose   : Dismount and deallocate tape drive
$!
$! Arguments : P1 = logical name of tape drive
$!             P2 = NOUNLOAD/[UNLOAD]
$!
$! Created  13-NOV-1991   Alan M. Jonckheere
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$!
$   IF f$extract(0,1,p2) .eqs. "N"
$   THEN
$       swit = "NOUNLOAD"
$   ELSE
$       swit = "UNLOAD"
$   ENDIF
$   IF f$trnlnm("''p1'") .nes. ""
$   THEN
$       dismount/'swit' 'p1'
$       deallocate 'p1'
$       deass 'p1'
$   ENDIF
$!
$EXIT:
$   EXIT
