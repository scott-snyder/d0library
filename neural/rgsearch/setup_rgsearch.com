$!========================================================================
$!
$! Name      : SETUP_RGSEARCH
$!
$! Purpose   : Define basic logicals and symbols for RGSEARCH.EXE
$!
$! Arguments : 
$!
$! Created  1-MAY-1995   Harrison B. Prosper, Chip Stewart
$! Modified 13-JAN-1996   Harrison B. Prosper 
$!  Get ready for a release
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$   
$   DEFINE/NOLOG    RGSEARCH_RCP    RGSEARCH.RCP
$   RGS*EARCH       :== RUN D0$NEURAL:RGSEARCH.EXE
$   WRITE SYS$OUTPUT "  "
$   WRITE SYS$OUTPUT "  Type"
$   WRITE SYS$OUTPUT "      RGS          To run RGSEARCH.EXE"
$   WRITE SYS$OUTPUT "      RGSEARCH_RCP points to your local RGSEARCH.RCP file"
$   
$EXIT:
$   EXIT
