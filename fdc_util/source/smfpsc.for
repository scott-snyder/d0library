      SUBROUTINE SMFPSC(LKFPSC,NEL,NWORDS,CONT)
C------------------------------------------------------------------------
C
C  Fetch Zebra bank FPSC (bank of of 'ideal' hits from Geant) and smear
C  the drift coordinate
C
C  Input:  LKFPSC  = location of bank 'FPSC'
C
C  Output: CONT   = modified bank contents
C                   (smeared drift time and distance and )
C
C-   Created  xx-DEC-1990   Daria Zieminska 
C-   Updated  28-FEB-1990   Jeffrey Bantly  clean up 
C
C------------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
      INTEGER NHIT,LKFPSC,IHIT,LOCHIT,NEL,NWORDS
      REAL SIG,DRIFT,RANDOM
      REAL CONT(18*MX_HIT_PSEC)
C------------------------------------------------------------------------
      NHIT=IQ(LKFPSC+1)
      DO 100 IHIT=1,NHIT
        LOCHIT=LKFPSC+2*NEL+3+NWORDS*(IHIT-1)
        DRIFT=Q(LOCHIT+2)
        IF (DRIFT.LT.0.025) GO TO 100
        SIG=Q(LOCHIT+5)
        CALL NORRAN(RANDOM)
        Q(LOCHIT+2)=Q(LOCHIT+2)+RANDOM*SIG    ! smear drift coordinate
        Q(LOCHIT+3)=Q(LOCHIT+3)-RANDOM*SIG    ! on both sides
  100 CONTINUE
      CALL UCOPY(Q(LKFPSC+2*NEL+4),CONT,NWORDS*NHIT)
C------------------------------------------------------------------------
      RETURN
      END
