$!------------------------------------------------
$!
$! Name      : SETUP_HMATRIX.COM
$!
$! Purpose   : Setup some CALOR_OFF commands
$!
$! Arguments :  'P1' = SERIAL NUMBER OF JOB = ETA INDEX
$!
$! Created 20-DEC-1990   Rajendran Raja
$!
$!------------------------------------------------
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$
$   access electron_id
$   DEFINE HMATRIX_SAVE    	D0$CALOR_OFF:HMATRIX_ELECTRON_SAVE.DAT
$   DEFINE HMATRIX_RCP          D0$CAL:LJTOP_HMATRIX_PACKAGE.RCP
$WRITE SYS$OUTPUT " H MATRIX package rcp SETUP"
$
$EXIT:
$   EXIT
