$!========================================================================
$! Name      : BUILD_OBJs
$!             A small piece extracted from BUILD_VMS_EXE
$!             The full use of BUILD_VMS_EXE requires release
$!             of too many other libraries for now.
$!
$! Note      : VMS_FILTER_PARAMETERS, VMS_FILTER_INIT created, by hand, thru
$!             run of FILTER_MAKER,  and reflect the most current L2TOOL.DAT.
$!             These  FOR  files are placed in the CMS group L2SIM, compiled 
$!             upon release (see below), with resulting  OBJs  kept in L2SIM.
$!
$! Arguments : p1 = SKIP will call  L2LIB_SETUP,  but perform no LIBTESTing
$!                  (option required when executing under release procedure)
$!
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$   SAY :== WRITE SYS$OUTPUT
$!                                              ! Work through the LOGICAL
$   RELtest = F$TRNLNM("D0$RELEASE")            ! that must be used 
$   IF RELtest.EQS.""                           ! during RELEASE procedures
$!
$     THEN L2ROOT = F$TRNLNM("D0$LEVEL2$ROOT") - "LEVEL2.]" + "]"
$     DEFINE/NOLOG D0$RELEASE/TRANS=(CONCEALED,TERMINAL) 'L2ROOT'
$!
$   ENDIF
$!
$   @D0$RELEASE:[LEVEL2.COMMAND_PROC]L2LIB_SETUP 'p1'
$!                                                         
$   SET DEFAULT D0$RELEASE:[LEVEL2.L2SIM]
$   OBJtest = F$SEARCH("VMS_FILTER_PARAMETERS.OBJ")
$   IF OBJtest.EQS."" 
$     THEN FOR VMS_FILTER_PARAMETERS
$   ENDIF
$   OBJtest = F$SEARCH("DEB_VMS_FILTER_PARAMETERS.OBJ")
$   IF OBJtest.EQS."" 
$     THEN FOR/NOOPT/DEBUG/OBJ=DEB_VMS_FILTER_PARAMETERS.OBJ  -
      VMS_FILTER_PARAMETERS
$   ENDIF
$!
$   OBJtest = F$SEARCH("VMS_FILTER_INIT.OBJ")
$   IF OBJtest.EQS."" 
$     THEN FOR VMS_FILTER_INIT
$   ENDIF
$   OBJtest = F$SEARCH("DEB_VMS_FILTER_INIT.OBJ")
$   IF OBJtest.EQS."" 
$     THEN FOR/NOOPT/DEBUG/OBJ=DEB_VMS_FILTER_INIT.OBJ VMS_FILTER_INIT
$   ENDIF
$!
$   OBJtest = F$SEARCH("VMS_FILTER_STP.OBJ")
$   IF OBJtest.EQS."" 
$     THEN FOR VMS_FILTER_STP
$   ENDIF
$   OBJtest = F$SEARCH("DEB_VMS_FILTER_STP.OBJ")
$   IF OBJtest.EQS."" 
$     THEN FOR/NOOPT/DEBUG/OBJ=DEB_VMS_FILTER_STP.OBJ VMS_FILTER_STP
$   ENDIF
$!
$   OBJtest = F$SEARCH("VMS_FILTER_D0USER.OBJ")
$   IF OBJtest.EQS."" 
$     THEN FOR VMS_FILTER_D0USER
$   ENDIF
$   OBJtest = F$SEARCH("DEB_VMS_FILTER_D0USER.OBJ")
$   IF OBJtest.EQS."" 
$     THEN FOR/NOOPT/DEBUG/OBJ=DEB_VMS_FILTER_D0USER.OBJ  -
      VMS_FILTER_D0USER
$   ENDIF
$!
$EXIT:
$!   SET DEFAULT D0$RELEASE:[LEVEL2]
$   EXIT
