$!========================================================================
$!
$! Name      : BUILD_NEURAL_BUILD
$!
$! Purpose   : Create NEURAL_BUILD.EXE
$!
$! Arguments : None
$!
$! Created   2-FEB-1995   Harrison B. Prosper
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$   
$   SET VERIFY
$   LINK/EXE=D0$NEURAL:NEURAL_BUILD -
        D0$NEURAL:NEURAL.OLB/INCLUDE=(NEURAL_BUILD), -
        D0$NEURAL:NEURAL.OPT/OPT,-
        D0$CERNLIB$SRC:VMSLIB_M.OPT/OPT
$   
$EXIT:
$   SET NOVERIFY
$   EXIT
