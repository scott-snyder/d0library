$!========================================================================
$! Name      : BUILD_STANDARD_FILES
$!           << COM file to be run by release procedure >>
$!
$! Purpose   : To build an official "standard"  .EXE and .STPs
$!              for a level 2 release
$!
$! Note      : VMS_FILTER_PARAMETERS, VMS_FILTER_INIT created, by hand, thru
$!             run of FILTER_MAKER,  and reflect the most current L2TOOL.DAT.
$!             These  FOR  files are placed in the CMS group L2SIM, compiled
$!             upon release (see below), with resulting  OBJs  kept in L2SIM.
$!
$! Arguments : p1       = EXE(s) [def]  build new EXEs only
$!                      = STP(s)        build only new STP.EXE and STPs
$!                      = ALL           build a full set of EXEs and new STPs
$!             p2       = VMS    [def]  build chosen for VMS
$!                      = type          build chosen for ELN node type 'type'
$!             p3       = NOSETUP       don't call l2lib_setup again
$! Created   6-JAN-1992   Daniel R. Claes
$!           4-JAN-1993   DRC  -  generalized for PRODUCTION area/BETA area
$!                                as well as official and GAMMA releases
$! Modified 20-JAN-1993   James T. Linnemann allow ELN as well; 
$!                  delegate OBJ building, and handling of TEMP dir for working
$! BUILD_XXX.COM's : use TEMP_DIR(may delete), do own renaming, return status
$!          23-JUN-1993   DRC - patch and keep the new VMS_FILTER_D0USER.COM 
$!          09-MAY-1993   DRC - copy VMS frame code for LOCAL_PASS to capture
$!========================================================================
$   ON ERROR     THEN $ GOTO ERROR_EXIT
$   ON CONTROL_Y THEN $ GOTO CONTY_EXIT
$   SAY :== WRITE SYS$OUTPUT
$   say "*** Start BUILD_STANDARD_FILES.COM ''p1' ''p2' ''p3' ***"
$   SDIR = F$ENVIRONMENT("DEFAULT") ! Starting directory
$!
$   EXE = (p1.EQS."").OR.(P1.EQS."EXE").OR.(P1.EQS."EXES").OR.(P1.EQS."ALL")
$   STP = (P1.EQS."STP").OR.(P1.EQS."STPS").OR.(P1.EQS."ALL")
$   VMS = (P2.EQS."").OR.(P2.EQS."VMS")
$!
$   d0$status :== TRUE
$!
$   IF (VMS) THEN @D0$LEVEL2$COMMAND_PROC:BUILD_VMS_OBJS.COM NOSETUP
$   if .not.d0$status then goto ERROR_EXIT
$!
$   @D0$LEVEL2$COMMAND_PROC:TEMP_DIR SET_DEFAULT
$   if .not.d0$status then goto ERROR_EXIT
$!
$   IF (EXE)
$   THEN
$       IF (P3.NES."NOSETUP") THEN @D0$LEVEL2$COMMAND_PROC:L2LIB_SETUP SKIP
$       COPY D0$LEVEL2$L2SIM:D0_FILTER.OPT *
$       COPY D0$LEVEL2$L2SIM:DEB_D0_FILTER.OPT *
$       IF (VMS)
$       THEN
$!$           PBD/FRAME=D0USER/PACK=GRAND_FSUM-
$!               /NAME=GRAND_FSUM/COMPILE/ZEBCOM=800000/ZEBSTP=700000
$!$           @GRAND_FSUM_D0USER.LNK
$!$           @GRAND_FSUM_D0USER.LNK DEBUG
$
$           COPY D0$LEVEL2$L2SIM:VMS_FILTER_INIT.FOR  -
                  'DEF_DIR'VMS_FILTER_INIT.FOR
$           COPY D0$LEVEL2$L2SIM:VMS_FILTER_PARAMETERS.FOR -
                  'DEF_DIR'VMS_FILTER_PARAMETERS.FOR
$           
$           @D0$LEVEL2$COMMAND_PROC:L2_VMS.LNK STANDARD
$           say "*** Start VMS_FILTER_D0USER.LNK DEBUG ***"
$           @VMS_FILTER_D0USER.LNK DEBUG
$           say "*** End VMS_FILTER_D0USER.LNK DEBUG ***"
$           RENAME VMS_FILTER_D0USER.FOR 'L2_DESTDIR'
$           say "*** Start VMS_FILTER_D0USER.COM """" """" """" DEFAULT ***"
$           @VMS_FILTER_D0USER.COM "" "" "" DEFAULT
$           say "*** End VMS_FILTER_D0USER.COM """" """" """" DEFAULT ***"
$
$           OPEN/READ/ERROR=OPEN_ERR OLD_FILE VMS_FILTER_D0USER.COM
$           OPEN/WRITE NEW_FILE VMS_FILTER_D0USER.COM
$READ_LOOP:
$           READ  OLD_FILE LINE
$           WRITE NEW_FILE LINE
$           IF f$locate("$ IF F$SEARCH",LINE).NE.0 THEN GOTO READ_LOOP
$! Pick up one more line past this point
$           READ  OLD_FILE LINE
$           WRITE NEW_FILE LINE
$!leave NEW_FILE open so new info can be appended
$           WRITE NEW_FILE "   FILTER_DEFAULT:VMS_FILTER 'P1' 'P2' 'P3'"
$           CLOSE NEW_FILE
$OPEN_ERR:
$           CLOSE OLD_FILE
$           PURGE VMS_FILTER_D0USER.COM
$
$           RENAME VMS_FILTER_D0USER.COM 'L2_DESTDIR'
$       ELSE      
$!ELN link
$! *** Changed D0$LEVEL2$L2SIM -> D0$LEVEL2$SOURCE below - AMJ 12/22/93
$           COPY D0$LEVEL2$SOURCE:'P2'_FILTER_INIT.FOR *
$           COPY D0$LEVEL2$SOURCE:'P2'_FILTER_PARAMETERS.FOR *
$           say "*** Start L2_MAIN.LNK ''p2' DEBUG ***"
$           @D0$D0DAQ:L2_MAIN.LNK 'P2' DEBUG
$           say "*** End L2_MAIN.LNK ''p2' DEBUG ***"
$           RENAME *.CODE 'L2_DESTDIR'
$           RENAME *.MAP 'L2_DESTDIR'
$       ENDIF
$       RENAME *.EXE 'L2_DESTDIR'
$   ENDIF
$!
$   IF (STP) THEN @D0$LEVEL2$COMMAND_PROC:BUILD_STPS.COM 'P2' 'P3'
$   if .not.d0$status then goto ERROR_EXIT
$!
$   @D0$LEVEL2$COMMAND_PROC:TEMP_DIR DELETE
$   if .not.d0$status then goto ERROR_EXIT
$
$EXIT:
$   d0$status :== TRUE
$   SET DEFAULT 'sdir'
$   say "*** End BUILD_STANDARD_FILES.COM ''p1' ''p2' ''p3' ***"
$   EXIT
$ERROR_EXIT:
$CONTY_EXIT:
$   d0$status :== FALSE
$   SET DEFAULT 'sdir'
$   say "*** End BUILD_STANDARD_FILES.COM ''p1' ''p2' ''p3' -ERROR- ***"
$   EXIT
