$!========================================================================
$!
$! Name      : GMHINT
$!
$! Purpose   : Type GM commands
$!
$! Arguments : 
$!
$! Created  29-JAN-1994   Harrison B. Prosper
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT

$   WRITE SYS$OUTPUT "  "
$   WRITE SYS$OUTPUT "  Type"
$   WRITE SYS$OUTPUT "  "
$   WRITE SYS$OUTPUT  -
    "      (1) CREATE_LIST     to create a Datafile List (GMCL*IST)"
$   WRITE SYS$OUTPUT "  "
$   WRITE SYS$OUTPUT  -
    "      (2) DEFINE_LIST     to define GM logicals     (GMDL*IST)"
$   WRITE SYS$OUTPUT "  "
$   WRITE SYS$OUTPUT  -
    "      (3) CREATE_NTUPLE   to create Ntuple          (GM)"
$   WRITE SYS$OUTPUT "  "
$   WRITE SYS$OUTPUT  -
    "      (4) DISPLAY_NTUPLE  to display Ntuple         (GMDI*SP)"
$   WRITE SYS$OUTPUT "  "
$   GMOUT  = F$TRNLNM("GM$IN")
$   WRITE SYS$OUTPUT "  "
$   WRITE SYS$OUTPUT "  Input from"
$   WRITE SYS$OUTPUT "  "
$   WRITE SYS$OUTPUT "     (GM$IN)  ''GMOUT'"
$   WRITE SYS$OUTPUT "  "
$   GMOUT  = F$TRNLNM("GM$OUT")
$   WRITE SYS$OUTPUT "  Output will appear in"
$   WRITE SYS$OUTPUT "  "
$   WRITE SYS$OUTPUT "     (GM$OUT) ''GMOUT'"
$   WRITE SYS$OUTPUT "  "
$   WRITE SYS$OUTPUT "  PAW KUMACs and COMIS routines are in"
$   WRITE SYS$OUTPUT "  "
$   WRITE SYS$OUTPUT "              GM$PAW"
$   WRITE SYS$OUTPUT "  "
$   WRITE SYS$OUTPUT  -
    "       (5) GMHINT          to TYPE this page"
$   WRITE SYS$OUTPUT "  "
$  
$EXIT:
$   EXIT
