$!========================================================================
$!
$! Name      : BACKUP_SHOWERLIB
$!
$! Purpose   : Backs up SHOWERlibrary to Tape 
$!
$! Arguments : 'P1' = TAPE LABEL (EG. BLND10)
$!             'P2' = DRIVE (EG. MUA0:, MKB100)
$!             'P3' = SAVE SET NAME (EXTENSION WILL BE .BCK)
$!
$! Created   6-JUL-1990   Rajendran Raja
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$A:
$ALLOCATE 'P2': 'p1'
$IF $STATUS.EQ.1 THEN GOTO B
$IF $STATUS.EQ.%x00030001 THEN GOTO B
$WAIT 00:00:15
$GOTO A
$!
$B:
$   IF P3 .EQS. ""
$   THEN P3 = "SHOWERLIBRARY"
$   ENDIF
$Mount/for 'p2'
$backup/rewind/log/ignore=(interlock,label) -
 d0sf13$dkb0:[shower_library]*.*,-
 d0sf13$dkb0:[d0geant]*.* 'p1':'P3'.bck
$EXIT:
$   EXIT
