      SUBROUTINE MUPRTD(UNIT)
CC     =====================================
CC     CHANGEABLE, USER ROUTINE TO PRINT OUT STUFF FROM EVENT DISPLAY
CC     DH 1-1-86
CC     DH 4/90 USES GTSRCP TO CONTROL
CC     =========================================
      IMPLICIT NONE     
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER UNIT,IRUN,IEVNT
      IRUN = IQ(LHEAD+6)
      IEVNT = IQ(LHEAD+9)
      WRITE(UNIT,101) IRUN,IEVNT
  101 FORMAT(' RUN,EVENT NUMBER = ',2I8)
CC
CC     1=PRINT UNIT   2=LINK   3=BANK NUMBER  4='ALL','LINEAR','ONE'
CC     5=(0=COMPLETE  1=PARTIAL)
      CALL PRMUD1(UNIT,0,0,'NOTHING',0)   ! PRINT PROCESSED
      CALL PRMUOH(UNIT,0,0,'NOTHING',0)   ! PRINT PROCESSED
      CALL PRMUOT(UNIT,0,0,'ALL',0)       ! PRINT MUON TRACKS   
      RETURN
      END
