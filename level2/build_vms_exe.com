$!========================================================================
$! Name      : BUILD_VMS_EXE
$!           << COM file to be run by release procedure >>
$!
$! Purpose   : To build an official "standard" VMS_FILTER.EXE that
$!             can be copied and used by users running SETUP_L2SIM.  
$!             This package includes CALOR and L1SIM. (CAHITS removed 11-FEB)
$!             Also builds VMS_FILTER_STP and generates STP files.
$!
$! Note      : VMS_FILTER_PARAMETERS, VMS_FILTER_INIT created, by hand, thru
$!             run of FILTER_MAKER,  and reflect the most current L2TOOL.DAT.
$!             These  FOR  files are placed in the CMS group L2SIM, compiled 
$!             upon release (see below), with resulting  OBJs  kept in L2SIM.
$!
$! Arguments : p1 = SKIP will call  L2LIB_SETUP,  but perform no LIBTESTing
$!                  (option required when executing under release procedure)
$!
$! Created   6-JAN-1992   Daniel R. Claes
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$   SAY :== WRITE SYS$OUTPUT
$!                                              ! Check if COM file
$   RELtest = F$TRNLNM("D0$RELEASE")            ! is being run by
$   IF RELtest.EQS.""                           ! release procedure
$!
$     THEN @D0$LEVEL2$ROOT:[COMMAND_PROC]L2LIB_SETUP 'p1'
$!                                                      
$     L2ROOT = F$TRNLNM("D0$LEVEL2$ROOT") - "LEVEL2.]" + "]"
$     DEFINE/NOLOG D0$RELEASE/TRANS=(CONCEALED,TERMINAL) 'L2ROOT'
$!
$   ELSE @D0$RELEASE:[LEVEL2.COMMAND_PROC]L2LIB_SETUP 'p1'
$!
$   ENDIF
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
$   OBJtest = F$SEARCH("VMS_FILTER_INIT.OBJ")
$   IF OBJtest.EQS."" 
$     THEN FOR VMS_FILTER_INIT
$   ENDIF
$   OBJtest = F$SEARCH("DEB_VMS_FILTER_INIT.OBJ")
$   IF OBJtest.EQS."" 
$     THEN FOR/NOOPT/DEBUG/OBJ=DEB_VMS_FILTER_INIT.OBJ VMS_FILTER_INIT
$   ENDIF
$   SET DEFAULT D0$RELEASE:[LEVEL2]
$!
$   DIRtest = F$SEARCH("TEMP.DIR")
$   IF DIRtest.EQS."" THEN $CREATE/DIRECTORY [.TEMP]
$   SET DEFAULT [.TEMP]
$   @D0$RELEASE:[LEVEL2.COMMAND_PROC]L2LIB_SETUP 'p1'
$   COPY D0$RELEASE:[LEVEL2.L2SIM]D0_FILTER.OPT *
$   COPY D0$RELEASE:[LEVEL2.L2SIM]DEB_D0_FILTER.OPT *
$   COPY D0$RELEASE:[LEVEL2.L2SIM]D0_FILTER_STP.OPT *
$   COPY D0$RELEASE:[LEVEL2.L2SIM]DEB_D0_FILTER_STP.OPT *
$   PBD/FRAME=D0USER/PACK=(CALOR,L1SIM,VMS_FILTER)-
       /NAME=VMS_FILTER/COMPILE/ZEBCOM=800000/ZEBSTP=700000
$   @VMS_FILTER_D0USER
$   @VMS_FILTER_D0USER.LNK
$   RENAME VMS_FILTER_D0USER.EXE [-]
$   @VMS_FILTER_D0USER.LNK DEBUG
$   RENAME VMS_FILTER_DEB_D0USER.EXE [-]
$   RENAME VMS_FILTER_D0USER.FOR D0$RELEASE:[LEVEL2.L2SIM]
$   RENAME VMS_FILTER_D0USER.OBJ D0$RELEASE:[LEVEL2.L2SIM]
$   RENAME DEB_VMS_FILTER_D0USER.OBJ D0$RELEASE:[LEVEL2.L2SIM]
$!   RENAME VMS_FILTER_D0USER.COM D0$RELEASE:[LEVEL2.COMMAND_PROC]
$!   
$   COPY D0$RELEASE:[LEVEL2.COMMAND_PROC]L2LIB_SETUP.COM *
$! prepare for changing this logical to something more useful
$   DEFINE VMS_FILTER_STP NL:   
$   @D0$RELEASE:[LEVEL2.COMMAND_PROC]NEW_STP 1 LINK
$   RUN_STP
$   RENAME VMS_FILTER.STP D0$RELEASE:[LEVEL2.L2SIM]VMS_FILTER_1.STP
$   CHANGE_VERSION 2 STP
$   @L2RCP_SETUP
$   RUN_STP
$   RENAME VMS_FILTER.STP D0$RELEASE:[LEVEL2.L2SIM]VMS_FILTER_2.STP
$   RENAME VMS_FILTER_STP.EXE [-]
$   RENAME VMS_FILTER_STP.FOR D0$RELEASE:[LEVEL2.L2SIM]
$   DELETE/NOCONFIRM/EXCLUDE=(L2LIB_SETUP.COM,DEB_D0_FILTER_STP.OPT) *.*;*
$   @D0$RELEASE:[LEVEL2.COMMAND_PROC]NEW_STP 1 DEBUG LINK
$   RENAME VMS_FILTER_STP.EXE [-]DEB_VMS_FILTER_STP.EXE
$   DELETE/NOCONFIRM *.*;*
$   SET DEFAULT [-]
$   SET FILE/PROTECTION=(O:RWED) TEMP.DIR
$   SET ACL/DEFAULT TEMP.DIR
$   DELETE/NOCONFIRM TEMP.DIR;*
$EXIT:
$!   SET DEFAULT D0$RELEASE:[LEVEL2.000000]
$   EXIT
