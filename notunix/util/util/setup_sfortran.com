$!========================================================================
$!
$! Name      : SETUP_SFORTRAN
$!
$! Purpose   : Setup commands SFORTRAN and SFO.
$!
$! Arguments : 
$!
$! Created   7-OCT-1990   Harrison B. Prosper
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$   
$   SFOR*TRAN   :== $D0$UTIL:SFORTRAN
$   SFO         :== @D0$UTIL:SFO
$   
$EXIT:
$   EXIT
