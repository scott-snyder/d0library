\$!========================================================================
$!
$! Name      : NEW_STP
$!
$! Purpose   : Copy and compile VMS_FILTER_STP code, LINK and RUN
$!             For users needing to generate a new VMS_FILTER.STP
$!             due to modifying downloaded  Tool_INIT  parameters
$! 'DEF_DIR' is [] for this routine!!!!! JTL  (.LNK doesn't use DEF_DIR)
$!
$! Arguments : p1 = MC version number   1 : series M or earlier
$!                                      2 : series N or  later
$!             p1 or p2 or p3 = DEBUG (optional)
$!                              LINK  (optional) automatically re-LINK
$!
$! Created  12-NOV-1991   Daniel R. Claes
$!
$!========================================================================
$  ON ERROR     THEN $ GOTO EXIT
$  ON CONTROL_Y THEN $ GOTO EXIT
$  SAY :== WRITE SYS$OUTPUT
$!                                              ! Work thru same LOGICAL
$   RELtest = F$TRNLNM("D0$RELEASE")            ! forced  to use  within
$   IF RELtest.EQS.""                           ! the release  procedure
$     THEN L2ROOT = F$TRNLNM("D0$LEVEL2$ROOT") - "LEVEL2.]" + "]"
$     DEFINE/NOLOG D0$RELEASE/TRANS=(CONCEALED,TERMINAL) 'L2ROOT'
$   ENDIF
$!                                                                          
$  DEF_DIR = F$ENVIRONMENT( "DEFAULT" )
$  DEB = ""
$  RELINK = ""
$  IF p1.EQS.""
$    THEN p1 = "3"
$    GOTO START
$  ENDIF
$  IF p1.EQS."DEBUG" .OR. p1.EQS."DEB"
$    THEN p1 = "3"
$    GOTO DBG
$  ELSE SAY " "
$    IF p2.EQS."DEBUG" .OR. p2.EQS."DEB"
$      THEN GOTO DBG
$    ELSE SAY " "
$      IF p3.EQS."DEBUG" .OR. p3.EQS."DEB"
$        THEN GOTO DBG
$      ENDIF
$    ENDIF
$  ENDIF
$  GOTO NEXT
$DBG:
$  DEB = "DEB_"
$  p4 = "DEBUG"
$NEXT:
$  IF p1.EQS."LINK"
$    THEN p1 = "3"
$    GOTO REL
$  ELSE SAY " "
$    IF p2.EQS."LINK" 
$      THEN GOTO REL
$    ELSE SAY " "
$      IF p3.EQS."LINK" 
$        THEN GOTO REL
$      ENDIF
$    ENDIF
$  ENDIF
$  GOTO START
$REL:
$  RELINK = "YES"
$START:
$  @L2LIB_SETUP SKIP
$  @D0$RELEASE:[LEVEL2.COMMAND_PROC]CHANGE_VERSION 'p1' STP
$  @L2RCP_SETUP
$  IF RELINK .EQS. "YES"
$    THEN GOTO SKIP
$  ENDIF
$!
$  SAY " "
$  SAY "          Y - run  existing  EXE  from  official area"
$  SAY "          N - recompile/link new EXE in your own area"
$  INQUIRE/nopunct ans "Do you want to run the standard STP.EXE [Y]"
$!
$  IF (ans.EQS."Y" .OR. ans.EQS."")
$    THEN RUN_STP :== "$D0$LEVEL2:''deb'VMS_FILTER_STP.EXE"
$    GOTO INSTRUCTIONS
$  ENDIF
$SKIP:
$  DEASSIGN VMS_FILTER_STP            ! Logical name conflict w/ routine name
$  TEST = F$SEARCH("VMS_FILTER_STP.FOR")
$  IF TEST.EQS.""
$    THEN COPY - 
       D0$RELEASE:[LEVEL2.L2SIM]VMS_FILTER_STP.FOR 'DEF_DIR'VMS_FILTER_STP.FOR
$  ENDIF
$  TEST = F$SEARCH("'deb'VMS_FILTER_STP.OPT")
$  IF TEST.EQS.""
$    THEN COPY - 
       D0$RELEASE:[LEVEL2.L2SIM]D0_FILTER_STP.OPT 'DEF_DIR'D0_FILTER_STP.OPT
$    COPY D0$RELEASE:[LEVEL2.L2SIM]DEB_D0_FILTER_STP.OPT   - 
       'DEF_DIR'DEB_D0_FILTER_STP.OPT
$  ENDIF
$  @D0$FILTER_UTIL:FILTER_STP.LNK VMS 'p4'
$  TMP = DEF_DIR + DEB + "VMS_FILTER_STP.EXE"
$  RUN_STP :== "$''TMP'"
$INSTRUCTIONS:
$  DEFINE VMS_FILTER_STP 'DEF_DIR'VMS_FILTER.STP
$  OPEN/APPEND/ERROR=OPEN_ERR COM_FILE 'DEF_DIR'L2LIB_SETUP.COM
$  WRITE COM_FILE "$!"
$  WRITE COM_FILE "$ DEFINE VMS_FILTER_STP 'DEF_DIR'VMS_FILTER.STP"
$  CLOSE COM_FILE
$OPEN_ERR:
$ SAY " "
$ SAY "  $ RUN_STP       or       $ RUN_STP/NOSMG"
$ SAY "  to generate your new VMS_FILTER.STP file"
$!
$EXIT:
$   EXIT
