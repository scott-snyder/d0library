$!========================================================================
$!
$! Name      : BUILD_RGSEARCH
$!
$! Purpose   : Create RGSEARCH.EXE
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
$   FORTRAN D0$NEURAL$SOURCE:NEURAL_INPAWC.F
$   
$   LINK/EXE=D0$NEURAL:RGSEARCH.EXE NEURAL_INPAWC.OBJ,-
        D0$NEURAL:NEURAL.OLB/INCLUDE=(RGSEARCH), -
        D0$NEURAL:NEURAL.OPT/OPT,-
        D0$CERNLIB$SRC:VMSLIB_M.OPT/OPT
$   
$EXIT:
$   IF F$SEARCH("NEURAL_INPAWC.OBJ") .NES. ""
$   THEN
$       DELETE/LOG/NOCONFIRM NEURAL_INPAWC.OBJ;*
$   ENDIF
$   SET NOVERIFY
$   EXIT
