$!========================================================================
$!
$! Name      : NEW_STP
$!
$! Purpose   : Copy and compile VMS_FILTER_STP code, LINK and RUN
$!             For users needing to generate a new VMS_FILTER.STP
$!             due to modifying downloaded  Tool_INIT  parameters
$! 'DEF_DIR' is [] for this routine!!!!! JTL  (.LNK doesn't use DEF_DIR)
$!
$!                                      D : sets default Mu/SFTVSN for DATA
$! Arguments : p1 = MC version number   1 : series M or earlier
$!                                      2 : series N Monte Carlo (and data)
$!                                      3 : series P or Q
$!                                      4 : series R or any MC w/CADMAKE
$!             p2 = Muon verson         OLD : series N or earlier
$!                                      NEW : Series P,Q,R or special N
$!                                      SRV : Surveyed geometry for DATA
$!                                            or MC series S
$!             p1 or p2 or p3  or p4 =  DEBUG (optional)
$!                                      LINK  (optional) automatically re-LINK
$!                                      STAND (optional) automatically run the
$!                                                       stndrd FILTER_STP.EXE
$!
$! Created  12-NOV-1991   Daniel R. Claes
$!          23-JUL-1992   DRC - Add SRV option for DATA-only Muon STP file
$!          29-OCT-1992   DRC - STAND option selects STANDARD exe as default
$! Modified  2-FEB-1993   James T. Linnemann kill off deb_filter_stp.opt
$!========================================================================
$  ON ERROR     THEN $ GOTO EXIT
$  ON CONTROL_Y THEN $ GOTO EXIT
$  SAY :== WRITE SYS$OUTPUT
$  say "*** Start NEW_STP ''p1' ''p2' ''p3' ***"
$! 
$  @D0$LEVEL2$COMMAND_PROC:DEFINE_L2_PASS.COM
$!
$  DEF_DIR = F$ENVIRONMENT( "DEFAULT" )
$  STND =""
$  DEB = ""
$  RELINK = ""
$  IF p1.EQS.""
$  THEN
$      p1 = "9"
$      p2 = "???"
$      GOTO START
$  ENDIF
$  IF p1.EQS."DEBUG" .OR. p1.EQS."DEB"
$  THEN
$      p1 = "9"
$      p2 = "???"
$      GOTO DBG
$  ELSE
$      SAY " "
$      IF p2.EQS."DEBUG" .OR. p2.EQS."DEB"
$      THEN
$          p2 = "???"
$          GOTO DBG
$      ELSE
$          SAY " "
$          IF p3.EQS."DEBUG" .OR. p3.EQS."DEB"
$          THEN
$              GOTO DBG
$          ELSE
$              SAY " "
$              IF p4.EQS."DEBUG" .OR. p4.EQS."DEB"
$              THEN
$                  GOTO DBG
$              ENDIF
$          ENDIF
$      ENDIF
$  ENDIF
$  GOTO NEXT
$DBG:
$  DEB = "DEB_"
$  p5 = "DEBUG"
$NEXT:
$  IF p1.EQS."STAND"
$  THEN
$      p1 = "9"
$      p2 = "???"
$      STND = "YES"
$      GOTO START
$  ELSE
$      SAY " "
$      IF p2.EQS."STAND"
$      THEN
$          p2 = "???"
$          STND = "YES"
$          GOTO START
$      ELSE
$          SAY " "
$          IF p3.EQS."STAND"
$          THEN
$              STND = "YES"
$              GOTO START
$          ELSE
$              SAY " "
$              IF p4.EQS."STAND"
$              THEN
$                  STND = "YES"
$                  GOTO START
$              ENDIF
$          ENDIF
$      ENDIF
$  ENDIF
$  IF p1.EQS."LINK"
$  THEN
$      p1 = "9"
$      p2 = "???"
$      GOTO REL
$  ELSE
$      SAY " "
$      IF p2.EQS."LINK"
$      THEN
$          p2 = "???"
$          GOTO REL
$      ELSE
$          SAY " "
$          IF p3.EQS."LINK"
$          THEN
$              GOTO REL
$          ELSE
$              SAY " "
$              IF p4.EQS."LINK"
$              THEN
$                  GOTO REL
$              ENDIF
$          ENDIF
$      ENDIF
$  ENDIF
$  GOTO START
$REL:
$  RELINK = "YES"
$START:
$  @L2LIB_SETUP SKIP
$  @D0$LEVEL2$COMMAND_PROC:CHANGE_VERSION 'p1' 'p2' STP
$  @D0$LEVEL2$COMMAND_PROC:DEFINE_L2_PASS
$  IF RELINK .EQS. "YES"
$  THEN
$      GOTO SKIP
$  ENDIF
$  IF STND .EQS. "YES"
$  THEN
$      RUN_STP :== "$D0$LEVEL2:''deb'VMS_FILTER_STP.EXE"
$      GOTO EXIT
$  ENDIF
$!
$  SAY " "
$  SAY "          Y - run  existing  EXE  from  official area"
$  SAY "          N - recompile/link new EXE in your own area"
$  INQUIRE/nopunct ans "Do you want to run the standard STP.EXE [Y]"
$!
$  IF (ans.EQS."Y" .OR. ans.EQS."")
$  THEN
$      RUN_STP :== "$D0$LEVEL2$L2SIM:''deb'VMS_FILTER_STP.EXE"
$      GOTO INSTRUCTIONS
$  ENDIF
$SKIP:
$  STPtest = F$TRNLNM("VMS_FILTER_STP")
$  IF STPtest .EQS. "" THEN GOTO NODEASS
$  DEASSIGN VMS_FILTER_STP            ! Logical name conflict w/ routine name
$NODEASS:
$  TEST = F$SEARCH("VMS_FILTER_STP.FOR")
$  IF TEST.EQS.""
$  THEN
$      COPY D0$LEVEL2$L2SIM:VMS_FILTER_STP.FOR 'DEF_DIR'VMS_FILTER_STP.FOR
$  ENDIF
$  TEST = F$SEARCH("D0_FILTER_STP.OPT")
$  IF TEST.EQS.""
$  THEN
$      COPY D0$LEVEL2$L2SIM:D0_FILTER_STP.OPT 'DEF_DIR'D0_FILTER_STP.OPT
$  ENDIF
$  @D0$FILTER_UTIL:FILTER_STP.LNK VMS 'p5'
$  TMP = DEF_DIR + "VMS_FILTER_STP.EXE"
$  RUN_STP :== "$''TMP'"
$INSTRUCTIONS:
$ SAY " "
$ SAY "  $ RUN_STP       or       $ RUN_STP/NOSMG"
$ SAY "  to generate your new VMS_FILTER.STP file"
$!
$EXIT:
$   say "*** End NEW_STP ''p1' ''p2' ''p3' ***"
$   EXIT
