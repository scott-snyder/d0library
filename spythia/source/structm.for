C*********************************************************************
 
      SUBROUTINE STRUCTM(XX,QQ,UPV,DNV,USEA,DSEA,STR,CHM,BOT,TOP,GLU)
 
C...Dummy routine, to be removed when PDFLIB is to be linked.
      COMMON/LUDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      SAVE /LUDAT1/
      DOUBLE PRECISION XX,QQ,UPV,DNV,USEA,DSEA,STR,CHM,BOT,TOP,GLU
 
C...Stop program if this routine is ever called.
      WRITE(MSTU(11),5000)
      IF(RLU(0).LT.10.) STOP
      GLU=XX+QQ+UPV+DNV+USEA+DSEA+STR+CHM+BOT+TOP
 
C...Format for error printout.
 5000 FORMAT(1X,'Error: you did not link PDFLIB correctly.'/
     &1X,'Dummy routine STRUCTM in PYTHIA file called instead.'/
     &1X,'Execution stopped!')
 
      RETURN
      END
