$!========================================================================
$! Name      : BUILD_STPS
$!           << COM file to be run by release procedure >>
$!
$! Purpose   : Builds STP .EXE, generates STP files, and moves to L2_DESTdir
$!
$! Note      : VMS_FILTER_INIT is created, by hand, thru run of FILTER_MAKER,
$!             using the released  L2TOOL.DAT. The   FOR   file is placed in
$!             the CMS group  L2SIM, compiled upon release (see BUILD_OBJ or
$!             BUILD_VMS_EXE) and the  OBJ  kept in  L2SIM.
$!
$! Arguments    P1 =  [def] VMS for VMS .STPs
$!                          type  to build ELN STP for 'type'
$!              P2 =  NOSETUP   don't call l2lib_setup
$!
$! Created   28-OCT-1992   Daniel R. Claes - Extracted from BUILD_VMS_EXE
$! Modified   2-FEB-1993   James T. Linnemann kill off DEB_FILTER_STP.OPT
$!           25-JUN-1993   DRC - Add Muon SRV STP for Series S Monte Carlo
$!========================================================================
$   ON ERROR     THEN $ GOTO ERROR_EXIT
$   ON CONTROL_Y THEN $ GOTO CONTY_EXIT
$   SAY :== WRITE SYS$OUTPUT
$   say "*** Start BUILD_STPS ''p1' ''p2' ***"
$   SDIR = F$ENVIRONMENT("DEFAULT") ! Starting directory
$
$   VMS = (P1.EQS."").OR.(P1.EQS."VMS")
$
$!work in a .TEMP dir
$   d0$status :== TRUE
$   @D0$LEVEL2$COMMAND_PROC:TEMP_DIR SET_DEFAULT    
$   if .not.d0$status then goto ERROR_EXIT
$
$!set up defaults
$   COPY D0$LEVEL2$COMMAND_PROC:L2LIB_SETUP.COM *
$   IF (P2.NES."NOSETUP") THEN @L2LIB_SETUP SKIP                               
$   COPY D0$LEVEL2$L2SIM:D0_FILTER_STP.OPT *
$
$  IF (VMS)
$  THEN
$!    prepare for changing this logical to something more useful
$   DEFINE VMS_FILTER_STP NL:
$
$   NEW_STP 1 OLD LINK
$!Default...don't need.   CHANGE_VERSION 1 OLD STP
$   RUN_STP
$   RENAME VMS_FILTER.STP VMS_FILTER_1_OLD_MU.STP
$
$   CHANGE_VERSION 2 OLD STP
$   RUN_STP
$   RENAME VMS_FILTER.STP VMS_FILTER_2_OLD_MU.STP
$
$   CHANGE_VERSION 3 OLD STP
$   RUN_STP
$   RENAME VMS_FILTER.STP VMS_FILTER_3_OLD_MU.STP
$
$   CHANGE_VERSION 3 NEW STP
$   RUN_STP
$   RENAME VMS_FILTER.STP VMS_FILTER_3_NEW_MU.STP
$
$   CHANGE_VERSION 4 NEW STP
$   RUN_STP
$   RENAME VMS_FILTER.STP VMS_FILTER_4_NEW_MU.STP
$
$   CHANGE_VERSION 4 SRV STP
$   RUN_STP
$   RENAME VMS_FILTER.STP VMS_FILTER_4_SRV_MU.STP
$
$   CHANGE_VERSION D SRV STP
$   RUN_STP
$   RENAME VMS_FILTER.STP VMS_FILTER_D_SRV_MU.STP
$
$  ELSE     !for ELN nodes: use default RCPs
$!
$                               ! Pick up archived CALIB gain-corrected
$   DEFINE DBL3$CAL D0$STP$CAL  ! CAL DATA file.  CHANGE_VERSION makes
$                               ! this same definition for VMS version.
$   @D0$LEVEL2$COMMAND_PROC:L2RCP_SETUP
$   COPY D0$LEVEL2$L2SIM:'P1'_FILTER_STP.FOR *
$   WRITE SYS$OUTPUT "*** Start FILTER_STP.LNK ''p1' ***"
$   @D0$FILTER_UTIL:FILTER_STP.LNK 'P1'
$   WRITE SYS$OUTPUT "*** End FILTER_STP.LNK ''p1' ***"
$   WRITE SYS$OUTPUT "*** Run ''p1'_FILTER_STP ***"
$   RUN 'P1'_FILTER_STP.EXE
$  ENDIF
$  RENAME *.EXE 'L2_DESTdir'
$  RENAME *.STP 'L2_DESTdir'
$  @d0$level2$command_proc:TEMP_DIR DELETE
$
$EXIT:
$   DEASSIGN DBL3$CAL       ! go back to server for running sim
$   d0$status :== TRUE
$   SET DEFAULT 'sdir'
$   say "*** End BUILD_STPS ''p1' ''p2' ***"
$   EXIT
$ERROR_EXIT:
$CONTY_EXIT:
$   d0$status :== FALSE
$   SET DEFAULT 'sdir'
$   say "*** End BUILD_STPS ''p1' ''p2' -ERROR- ***"
$   EXIT
