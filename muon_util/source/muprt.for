      SUBROUTINE MUPRT(UNIT)
CC     =====================================
CC     CHANGEABLE, USER ROUTINE TO PRINT OUT STUFF
CC     DH 1-1-86
CC     =========================================
      IMPLICIT NONE     
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER UNIT,IRUN,IEVNT
      IRUN = IQ(LHEAD+6)
      IEVNT = IQ(LHEAD+9)
      WRITE(UNIT,101) IRUN
  101 FORMAT(' RUN NUMBER = ',I6)
      WRITE(UNIT,102) IEVNT
  102 FORMAT(' EVENT NUMBER = ',I6)
CC
CC     1=PRINT UNIT   2=LINK   3=BANK NUMBER  4='ALL','LINEAR','ONE'
CC     5=(0=COMPLETE  1=PARTIAL)
      CALL PRMUD1(UNIT,0,0,'NOTHING',0)   ! PRINT RAW
C      CALL PRMUOF(UNIT,0,0,'NOTHING',0)   ! PRINT FLAGS
      CALL PRMUOH(UNIT,0,0,'NOTHING',0)   ! PRINT PROCESSED
      CALL PRMUOT(UNIT,0,0,'ALL',0)       ! PRINT MUON TRACKS   
C      CALL PRMUHT(UNIT,0,0,'NOTHING',0)   ! PRINT MUON HIT HEADER
C      CALL PRMTRH(UNIT,0,0,'NOTHING',0)   ! PRINT MUON TRACK HEADER
CC
      RETURN
      END
