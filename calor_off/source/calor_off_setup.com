$!========================================================================
$!
$! Name      : CALOR_OFF_SETUP
$!
$! Purpose   : Does nothing (PBD compatible)
$!
$! Arguments : 
$!
$! Created  10-APR-1992   Chip Stewart
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$   WRITE SYS$OUTPUT "  ---- PBD SETUP done ----"
$EXIT:
$   EXIT
