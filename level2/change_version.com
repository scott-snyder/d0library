$!========================================================================
$!
$! Name      : CHANGE_VERSION   p1  (p2)
$!
$! Purpose   : Switch from one MC-version STP file to another
$!                               SFTVSN 1 data (series M or earlier)
$!                               SFTVSN 2 data (series N or later)
$!
$! Arguments : p1 = SFTVSN #
$!             p2 = "STP" for calls from within NEW_STP.COM
$!
$! Created  29-SEP-1991   James T. Linnemann - created as separate
$!                                             VSN1.COM & VSN2.COM
$!          15-OCT-1991   DC  switch to either MC series in single COM
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$   SAY :== WRITE SYS$OUTPUT
$!                                                                          
$START:
$ IF p1.EQS.""
$    THEN SAY " "
$    SAY "SFTVSN=1 (series M or earlier)"
$    SAY "SFTVSN=2 (series N or later)"
$    INQUIRE/NOPUNCT p1 "SFTVSN="
$ ENDIF
$ IF (p1.EQS."1").OR.(p1.EQS."2") THEN GOTO OK
$ p1 = ""
$ GOTO START
$OK:
$ IF p2.EQS."STP"
$   THEN TEST = F$SEARCH("L2RCP_SETUP.COM")
$!                                              ! Check if COM file is being
$   RELtest = F$TRNLNM("D0$RELEASE")            ! run by release procedure
$   IF RELtest.EQS.""
$     THEN L2ROOT = F$TRNLNM("D0$LEVEL2$ROOT") - "LEVEL2.]" + "]"
$     DEFINE/NOLOG D0$RELEASE/TRANS=(CONCEALED,TERMINAL) 'L2ROOT'
$   ENDIF
$!
$   IF TEST.EQS."" 
$     THEN COPY D0$RELEASE:[LEVEL2.:[COMMAND_PROC]L2RCP_SETUP.COM 'DEF_DIR'L2RCP_SETUP.COM
$   ENDIF
$   OPEN/APPEND/ERROR=OPEN_ERR   COM_FILE   'DEF_DIR'L2RCP_SETUP.COM
$   WRITE COM_FILE "$!"
$   WRITE COM_FILE "$ DEFINE CL2HITS_RCP  D0$LEVEL2$L2SIM:CL2HITS_''p1'.RCP"
$   CLOSE COM_FILE
$   GOTO EXIT
$OPEN_ERR:
$   SAY " "
$   SAY "Error opening L2RCP_SETUP.COM"
$   SAY "You need to DEFINE CL2HITS_RCP  D0$LEVEL2$L2SIM:CL2HITS_''p1'.RCP"
$   SAY "Before running VMS_FILTER_STP"
$   SAY " "
$ ELSE DEFINE VMS_FILTER_STP D0$LEVEL2$L2SIM:VMS_FILTER_'p1'.STP
$   OPEN/APPEND/ERROR=OPEN_ERR2 COM_FILE 'DEF_DIR'L2LIB_SETUP.COM
$   WRITE COM_FILE "$!"
$   WRITE COM_FILE "$ DEFINE VMS_FILTER_STP D0$LEVEL2$L2SIM:VMS_FILTER_''p1'.STP"
$   CLOSE COM_FILE
$   GOTO EXIT
$OPEN_ERR2:
$   SAY " "
$   SAY "Failed to find your L2LIB_SETUP.COM"
$   SAY "Be sure to DEF VMS_FILTER_STP D0$LEVEL2$L2SIM:VMS_FILTER_''p1'.STP"
$   SAY "Before any run of VMS_FILTER "
$   SAY " "
$ ENDIF
$EXIT:
$   EXIT
