$!========================================================================
$!
$! Name      : SETUP_GM
$!
$! Purpose   : Set up logicals for GM DST analysis using ntuples.
$!
$! Arguments : P1   INPUT AREA
$!             P2   OUTPUT AREA
$!             P3   MC or Data
$!
$! Created   4-NOV-1992   Harrison B. Prosper 
$! Modified 23-FEB-1993   Harrison B. Prosper 
$!  Add definition of CLEANEM_RCP
$! Modified 28-FEB-1993   Harrison B. Prosper 
$!  Add VTRAKS,FTRAKS
$! Modified  4-APR-1993   Harrison B. Prosper 
$!  REMOVE VTRAKS ETC.
$! Modified 11-MAY-1993   Harrison B. Prosper 
$!  Add definitions of GMDISP
$! 
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$   
$   Cluster = F$TRNLNM("SYS$CLUSTER_NODE")
$   IF Cluster .EQS. "D0::"
$   THEN
$       EXEDIR  = "ONLINE:[GM_EXEC.NEW]"
$       PAWDIR  = "ONLINE:[GM_EXEC.NEW.PAW]"
$   ELSE
$       EXEDIR  = "D0$PHYSICS_UTIL$ROOT:[GM]"
$       PAWDIR  = "D0$PHYSICS_UTIL$ROOT:[GMPAW]"
$   ENDIF
$   
$   GMIN   = P1
$   GMOUT  = P2
$   MC     = P3
$   IF MC .NES. ""
$   THEN
$       MC = "MC_"
$   ENDIF
$!================================================
$!   SET DI3000 DRIVERS
$!================================================
$   SETDRV XDW PST
$   
$!================================================
$!   DEFINE DIRECTORIES
$!================================================
$   
$   DEFDIR  = F$ENVIRONMENT("DEFAULT")
$   
$   INPDIR  = F$TRNLNM("GM$IN")
$   IF INPDIR .EQS. ""
$   THEN
$       INPDIR  = "D0$DATA$DST"
$   ENDIF
$   
$   OUTDIR  = F$TRNLNM("GM$OUT")
$   IF OUTDIR .EQS. ""
$   THEN
$       File = "USR$SCRATCH:[000000]''USERNAME'.DIR"
$       IF F$SEARCH(File) .NES. ""
$       THEN
$           OUTDIR = "USR$SCRATCH:[''USERNAME']"
$       ELSE
$           OUTDIR = DEFDIR
$       ENDIF
$   ENDIF
$   
$!================================================
$!   GET INPUT DIRECTORY
$!================================================
$   
$   IF GMIN .EQS. ""
$   THEN
$       INQUIRE GMIN "Directory containing Datafiles [''INPDIR'] "
$   ENDIF
$   IF GMIN .EQS. ""
$   THEN
$       GMIN = INPDIR
$   ENDIF
$   
$!================================================
$!   GET OUTPUT DIRECTORY
$!================================================
$   
$   IF GMOUT .EQS. ""
$   THEN
$       INQUIRE GMOUT "Directory for output files [''OUTDIR'] "
$   ENDIF
$   IF GMOUT .EQS. ""
$   THEN
$       GMOUT = OUTDIR
$   ENDIF
$   
$!================================================
$!   DEFINE LOGICALS
$!================================================
$   DEFINE/NOLOG    GM$IN      'GMIN'
$   DEFINE/NOLOG    GM$OUT     'GMOUT'
$   DEFINE/NOLOG    GM$EXE     'EXEDIR'
$   DEFINE/NOLOG    GM$DIR      [],'EXEDIR'
$   DEFINE/NOLOG    GM$PAW      [],'PAWDIR'
$   
$   DEFINE/NOLOG CALEVT_RCP         D0$CALOR_OFF:CALEVT.RCP
$   DEFINE/NOLOG CALFRAME_RCP       GM$DIR:GM_CALFRAME.RCP
$   DEFINE/NOLOG CALFRAME_RCPE      GM$DIR:GM_CALFRAME.RCPE
$   DEFINE/NOLOG PX_SYSTEM_RCP      D0$PIXIE:PX_SYSTEM.RCP
$   DEFINE/NOLOG PX_GM_DISPLAY_RCP  GM$DIR:PX_GM_DISPLAY.RCP
$   DEFINE/NOLOG GB_NTUPLES_RCP     GM$DIR:GB_NTUPLES.RCP
$   DEFINE/NOLOG GB_NTUPLES_RCPE    GM$DIR:GB_NTUPLES.RCPE
$   DEFINE/NOLOG GB_TRIGGER_RCP     GM$DIR:GB_TRIGGER.RCP
$!================================================
$!   Show location of RCP files
$!================================================
$   
$   WRITE SYS$OUTPUT "  "
$   @D0$BETA_UTIL:WHERE CALFRAME_RCPE
$   @D0$BETA_UTIL:WHERE GB_NTUPLES_RCPE
$   @D0$BETA_UTIL:WHERE GB_TRIGGER_RCP
$!================================================
$!   Send garbage to null
$!================================================
$   DEFINE/NOLOG    FOR003          NL:
$   DEFINE/NOLOG    ZEBRA           NL:
$   DEFINE/NOLOG    CALORIMETER_DMP NL:
$   DEFINE/NOLOG    CALORIMETER_OUT NL:
$   
$!================================================
$!   DEFINE SYMBOLS
$!================================================
$   CREATE_LIST     :== @GM$DIR:GM_CREATE_DATAFILE_LIST
$   DEFINE_LIST     :== @GM$DIR:GM_DEFINE_DATAFILE_LIST
$   CREATE_NTUPLE   :== RUN/NODEBUG GM$DIR:GM_CALOR_OFF
$   DISPLAY_NTUPLE  :== @GM$DIR:GMDISP.COM
$   
$   GMCL*IST        :== @GM$DIR:GM_CREATE_DATAFILE_LIST
$   GMDL*IST        :== @GM$DIR:GM_DEFINE_DATAFILE_LIST
$   GM              :== RUN/NODEBUG GM$DIR:GM_CALOR_OFF
$   GMDI*SP         :== @GM$DIR:GMDISP.COM
$   GMH*INT         :== @GM$DIR:GMHINT.COM
$   GMHINT
$   
$EXIT:
$   EXIT
