$!==========================================================================
$!
$! Name      : SETUP_L2SIM
$!
$! Purpose   : Copies to User a basic set of OPT files for linking
$!             and DAT files for running VMS_FILTER utilizing the
$!             TOOLS and parameters appropriate for a selected set
$!             of trigger definitions
$!
$!             Then it initiates Jan Hoftun's FILTER_MAKER dialog
$!             creating a new D0USER Program Builder executable
$!
$! Arguments : Specify the Data Definition Set and PBD Frame to build
$!             p1 = C(ALOR_OFF), D(0USER)    ******   NOT USED SEE 14-JUN-1994  ******
$!             p2 = Monte Carlo/DATA series being analyzed
$!             p3 = Muon STP constants set (OLD,NEW,SRV)
$!             P4 = destination directory of all these files
$!             P5 = 1   for running the standard prebuilt EXE
$!                  2   program build a customized VMS_FILTER
$!                  3   build_with/link_to L1SIM but not L2SIM 
$!                  4   no exe generated (only setup performed)
$!
$! Created  30-JUL-1991   Daniel R. Claes
$!          23-SEP-1991   Daniel R. Claes - modified to accomodate new
$!                                          VMS_FILTER test release
$!          20-NOV-1991   Daniel R. Claes - Frame choice & COOR_SIM files
$!          09-NOV-1992   Daniel R. CLaes - New SFTVSNs for MC series Q,R
$!          28-NOV-1992   James T. Linnemann stop copying CAHITS.RCP
$!          07-DEC-1992   Daniel R. Claes - rewritten w/o Trigger Defn Sets
$!           7-DEC-1992   James T. Linnemann stop all copying
$!          11-DEC-1992   James T. Linnemann P5 parameter
$!          17-APR-1993   Daniel R. Claes - Build L1SIM pkgs w/o VMS_FILTER
$!          23-JUN-1993   Daniel R. Claes - unconditionally @CHANGE_VERSION
$!          25-JUN-1993   Daniel R. Claes - New MU$CONST for MC Series S
$!          04-MAY-1994   Daniel R. Claes - COPY over L2SIM's VMS_FILTER_INIT/_PARAMETERS to 
$!                                          be CAPTURED by LOCAL_PASS (to avoid stray copies)
$!          14-JUN-1994   N. Varelas -  Remove the Frame choice question (=p1 variable) 
$!                                      and always use the D0USER Frame.  Rename the
$!                                      parameters p2,p3,p4,p5 to p1,p2,p3,p4, i.e.
$!             p1 = Monte Carlo/DATA series being analyzed
$!             p2 = Muon STP constants set (OLD,NEW,SRV)
$!             P3 = destination directory of all these files
$!             P4 = 1   for running the standard prebuilt EXE
$!                  2   program build a customized VMS_FILTER
$!                  3   build_with/link_to L1SIM but not L2SIM 
$!                  4   no exe generated (only setup performed)
$!==========================================================================
$  ON ERROR     THEN $ GOTO EXIT
$  ON CONTROL_Y THEN $ GOTO EXIT
$!
$  SAY :== WRITE SYS$OUTPUT
$  IF p3.EQS.""
$    THEN DEF_DIR = F$ENVIRONMENT( "DEFAULT" )
$  ELSE
$    DEF_DIR = p3
$  ENDIF
$!
$OK:
$  Series = p1
$  IF p1.EQS.""
$    THEN SAY " "
$    SAY                    "Analyzing real DATA   (D)"
$    SAY                    "or data with CADMAKE  (D)"
$    SAY                    "Any  MC with CADMAKE  (C)  or"
$    INQUIRE/NOPUNCT Series "Monte Carlo Series (N,P,Q,R,S)?"
$  ENDIF
$  IF (Series.EQS."D")
$      THEN SFTVSN = "D"
$      MUVER = "SRV"
$      RCP_TYPE = "DEFAULT"
$      GOTO OK3
$  ENDIF
$  RCP_TYPE = "MC"
$  IF (Series.EQS."N")
$      THEN SFTVSN = "2"
$      MUVER = "OLD"
$      GOTO OK3
$  ENDIF
$  IF ((Series.EQS."P").OR.(Series.EQS."Q"))
$      THEN SFTVSN = "3"
$      GOTO OK2
$  ENDIF
$  IF (Series.EQS."C")
$      THEN SFTVSN = "4"
$      GOTO OK2
$  ENDIF
$  IF (Series.EQS."R")
$      THEN SFTVSN = "4"
$      MUVER = "NEW"
$      GOTO OK3
$  ENDIF
$  IF (Series.EQS."S")
$      THEN SFTVSN = "4"
$      MUVER = "SRV"
$      GOTO OK3
$  ENDIF
$  p1 = ""
$ GOTO OK
$!
$OK2:
$  MUVER = p2
$  IF p2.EQS.""
$    THEN SAY " "
$    SAY                   "Select the Muon Constants"
$    SAY                   "Monte Carlo Constants (OLD,NEW)
$    INQUIRE/NOPUNCT MUVER "Data Survey Constants   (SRV) ?"
$  ENDIF
$  IF (MUVER.EQS."O") .OR. (MUVER.EQS."OLD")
$    THEN MUVER = "OLD"
$    GOTO OK3
$  ENDIF
$  IF (MUVER.EQS."N") .OR. (MUVER.EQS."NEW")
$    THEN MUVER = "NEW"
$    GOTO OK3
$  ENDIF
$  IF (MUVER.EQS."S") .OR. (MUVER.EQS."SRV")
$    THEN MUVER = "SRV"
$    GOTO OK3
$  ENDIF
$  IF (MUVER.EQS."D").OR.(MUVER.EQS."DAT").OR.(MUVER.EQS."DATA")
$    THEN MUVER = "SRV"
$    GOTO OK3
$  ENDIF
$  p2 = ""
$ GOTO OK2
$OK3:
$  TEST = F$SEARCH("L2LIB_SETUP.COM")
$  IF (TEST .EQS. "")
$    THEN GOTO COPYSETUP
$  ENDIF
$  T0 = F$FILE_ATTRIBUTES("D0$LEVEL2$COMMAND_PROC:L2LIB_SETUP.COM","CDT")
$  T0 = F$CVTIME(T0)
$  T1 = F$FILE_ATTRIBUTES("L2LIB_SETUP.COM","CDT")
$  T1 = F$CVTIME(T1)
$  IF (T0.GTS.T1)
$    THEN SAY "The OFFICIAL L2LIB_SETUP.COM is newer than your copy."
$    SAY "This file sets up the recommended LIBTEST for LINKing."
$    INQUIRE/NOPUNCT ans "Do you want the newer copy?"
$    IF (ans.EQS."Y") .OR. (ans.EQS."y")
$      THEN GOTO COPYSETUP
$    ENDIF
$  ENDIF
$  GOTO FOUNDsetup
$COPYSETUP:
$  COPY D0$LEVEL2$COMMAND_PROC:L2LIB_SETUP.COM    'DEF_DIR'L2LIB_SETUP.COM
$!  PURGE L2LIB_SETUP.COM
$FOUNDsetup:
$  DIFFerence/OUTPUT=NL: L2LIB_SETUP.COM D0$LEVEL2$COMMAND_PROC
$  private_l2lib = ($SEVERITY.NE.1)
$! need_change = private_l2lib.OR.(sftvsn.nes."D").OR.(muver.nes."SRV")
$! IF need_change THEN  @D0$LEVEL2$COMMAND_PROC:CHANGE_VERSION 'sftvsn' 'MUVER'  ! Appends STP/RCP definitions
$  @D0$LEVEL2$COMMAND_PROC:CHANGE_VERSION 'sftvsn' 'MUVER'  ! Appends STP/RCP definitions
$!
$        @L2LIB_SETUP.COM   "" "''RCP_TYPE'"
$   @d0$level2$command_proc:define_l2_pass
$!
$!
$PBD:
$  ans = P4
$  if ans.NES."" THEN GOTO CHECK_ANSWER
$!------------------------------------------------
$! Program Builder Dialogue taken from Jan Hoftun's
$! Name      : FILTER_MAKER
$! Purpose   : Make new version of VMS_FILTER_D0USER program
$!------------------------------------------------
$ SAY "Do you want: "
$ SAY "     1    To run the standard VMS_FILTER_D0USER.EXE"
$ SAY "       (Includes CALOR,CADMAKE,RERUN_L12,L1SIM,RECDDN)"
$ SAY "     2    Program Builder to make a private version"
$ SAY "             (For inclusion of your own packages)"
$ SAY "     3    Program Build a private package with L1SIM 
$ SAY "                 but NOT L2SIM (VMS_FILTER)"
$ SAY "     4    QUIT "
$ SAY " "
$ inquire/nopunct ans "    ?"
$CHECK_ANSWER:
$ IF ans.EQS."1"
$      THEN
$!              (VMS_FILTER_D0USER will simply point to the standard EXE)
$               COM_TEST = F$SEARCH("VMS_FILTER_D0USER.COM")
$               EXE_TEST = F$SEARCH("VMS_FILTER_D0USER.EXE")
$               DEB_TEST = F$SEARCH("VMS_FILTER_DEB_D0USER.EXE")
$               IF (COM_TEST.NES."").OR.(EXE_TEST.NES."").OR.(DEB_TEST.NES."")
$               THEN
$                   SAY " "
$                   SAY " *** WARNING!!!!  Found old VMS_FILTER_D0USER.COM/EXE in this directory. *** "
$                   SAY " *** Setup for running the STANDARD exe must be done in a new directory. *** "
$                   SAY " "
$               ENDIF
$          GOTO EXIT
$ ENDIF
$ IF (ans.EQS."2") .OR. (ans.EQS."3")
$      THEN 
$       IF (ans.EQS."2")
$           THEN 
$             LINKOPT = "VMS_FILTER"
$             COPY D0$LEVEL2$L2SIM:VMS_FILTER_INIT.FOR 'DEF_DIR'VMS_FILTER_INIT.FOR
$             COPY D0$LEVEL2$L2SIM:VMS_FILTER_PARAMETERS.FOR 'DEF_DIR'VMS_FILTER_PARAMETERS.FOR
$           ELSE
$             LINKOPT = "L1SIM"
$       ENDIF
$       @d0$level2$command_proc:l2_VMS.LNK 'LINKOPT'
$       @'LINKOPT'_d0user.com "" "" "" 'RCP_TYPE'
$       GOTO EXIT
$ endif
$ IF (ans.EQS."4")
$   THEN SAY "Building and compiling HSTRFL routine"
$   pbd/switch/frame=hstrfl/name=vms_filter/hstrfl/prod=2/pass='l2_pass_num'/ver='l2_version_num'/nocompile
$   for vms_filter_hstrfl.for
$   for/debug=all/noopt/obj=deb_vms_filter_hstrfl vms_filter_hstrfl.for
$   GOTO EXIT
$ ENDIF
$!
$ GOTO PBD          !illegal answer
$EXIT:
$   EXIT
