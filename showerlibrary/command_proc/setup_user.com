$!========================================================================
$!
$! Name      : SETUP_USER
$!
$! Purpose   : SETUP FILE FOR INPUT AND OUTPUT SPECIFICATIONS
$!
$! Arguments : P1,P2 INPUT AND OUTPUT FILE NAMES FOR FOR031 AND FOR032
$!             P3 = Geant control file name
$!
$! Created   6-NOV-1990   Rajendran Raja
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$   IF P1 .EQS. ""
$   THEN
$   P1 = "D0$D0GEANT$DATA:TTB_140.DAT"
$   ELSE
$   ENDIF
$
$
$   IF P2 .EQS. "" 
$   THEN
$   P2 = "TTB_140.GEN"
$   ELSE
$   ENDIF
$
$   IF P3 .EQS. ""
$   THEN
$   P3 = "D0$SHL:BAT1.DAT"
$   ELSE
$   ENDIF
$
$DEFINE FOR031 'P1'
$DEFINE FOR032 'P2'
$DEFINE FOR004 'P3'
$
$SHO LOGICAL FOR031
$SHO LOGICAL FOR032
$SHO LOGICAL FOR004
$
$WRITE SYS$OUTPUT " "
$WRITE SYS$OUTPUT " GEANT INPUT FILE DEFINED TO ''P1'"
$WRITE SYS$OUTPUT " GEANT OUTPUT FILE DEFINED TO ''P2'"
$WRITE SYS$OUTPUT " "
$
$WRITE SYS$OUTPUT " FFREAD INPUT FILE FILE DEFINED TO ''P3'"
$WRITE SYS$OUTPUT " type in READ 4 UPON RUNNING GEANT for FFREAD cards "
$WRITE SYS$OUTPUT " "
$
$EXIT:
$   EXIT
