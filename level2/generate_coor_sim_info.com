$!========================================================================
$! Name      : GENERATE_COOR_SIM_INFO
$!           << COM file to be run by release procedure >>
$!
$! Purpose   : To build the official COOR_SIM output files of INFO,
$!             DAT,  and ZDAT for the L1/L2 Simulation of all sets
$!             of default Trigger Sets:  Bland,  Dzero,  Grannis
$!
$! Created   3-JAN-1992   Daniel R. Claes
$!========================================================================
$   ON ERROR     THEN $ GOTO EXIT
$   ON CONTROL_Y THEN $ GOTO EXIT
$   SAY :== WRITE SYS$OUTPUT
$   SAY :== WRITE SYS$OUTPUT
$!                                              ! Check if COM file is being
$   RELtest = F$TRNLNM("D0$RELEASE")            ! run by release procedure
$   IF RELtest.EQS.""
$     THEN L2ROOT = F$TRNLNM("D0$LEVEL2$ROOT") - "LEVEL2.]" + "]"
$     DEFINE/NOLOG D0$RELEASE/TRANS=(CONCEALED,TERMINAL) 'L2ROOT'
$   ENDIF
$!
$   TRIG = "BLAND"
$   DEFINE/NOLOG TRIGset D0$CONFIGS$ROOT:[SOURCE]BLAND.CFG
$!
$   SET DEFAULT D0$RELEASE:[LEVEL2]
$   DIRtest = F$SEARCH("TEMP2.DIR")
$   IF DIRtest.EQS."" THEN $CREATE/DIRECTORY [.TEMP2]
$   SET DEFAULT [.TEMP2]
$   COPY D0$RELEASE:[LEVEL2.L2SIM]L2TOOL.DAT *
$   MISS = ""
$   @D0$RELEASE:[LEVEL2.COMMAND_PROC]L2LIB_SETUP SKIP
$START:
$   COOR_SIM
     D0$CONFIGS$ROOT:[SOURCE]COOR_SIM.CTL
     TRIGset
$! Need to wait for generation of ZDAT file
$   COUNT = 0
$ZDAT_WAIT:
$     WAIT 00:01:00.00  
$   Ztest = F$SEARCH("RCP_0000001.ZDAT")
$   TIME = F$TIME()
$   IF Ztest.EQS."" 
$     THEN SAY TIME
$     COUNT = COUNT + 1
$     IF COUNT.EQS.20 
$       THEN MISS = TRIG
$       GOTO NEXT
$     ENDIF
$     GOTO ZDAT_WAIT
$   ENDIF
$   @D0$RELEASE:[LEVEL2.COMMAND_PROC]UPDATE_COOR_INFO 'TRIG'
$NEXT:
$   IF TRIG.EQS."BLAND"
$     THEN TRIG="DZERO"
$     DEFINE/NOLOG TRIGset D0$CONFIGS$ROOT:[SOURCE]DZERO.CFG
$     GOTO START
$   ENDIF
$   IF TRIG.EQS."DZERO"
$     THEN TRIG="GRANNIS"
$     DEFINE/NOLOG TRIGset D0$CONFIGS$ROOT:[SOURCE]GRANNIS.CFG
$     GOTO START
$   ENDIF
$   DELETE/NOCONFIRM *.INFO;*
$   DELETE/NOCONFIRM *.DAT;*
$   DELETE/NOCONFIRM *.ERR;*
$   SET DEFAULT [-]
$   IF MISS.EQS.""
$     THEN $SET FILE/PROTECTION=(O:RWED) TEMP2.DIR
$     SET ACL/DEFAULT TEMP2.DIR
$     DELETE/NOCONFIRM TEMP2.DIR;*
$   ENDIF
$EXIT:
$   SET DEFAULT D0$RELEASE:[LEVEL2] 
$   EXIT
