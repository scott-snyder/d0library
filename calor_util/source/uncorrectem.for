      SUBROUTINE UNCORRECTEM(LPELC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : undo corrections using VCOR banks
C-
C-   Inputs  : LPELC - pointer to PELC or PPHO bank
C-   Outputs : none
C-   Controls: none
C-
C-   Created  31-MAR-1994   Ulrich Heintz - modified from Herb Greenlee's code
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LVCOR,ND,NL,LPELC
      REAL    SCALE_FACTOR
      LOGICAL BTEST
C
C ****  Undo CORRECTEM energy correction.
C
      ND = IQ(LPELC-1)
      NL = IQ(LPELC-3)
      LVCOR = LQ(LPELC-4)
      IF(ND.GE.31 .AND. NL.GE.4 .AND. LVCOR.GT.0 .AND. 
     &  BTEST(IQ(LPELC+31),0) .AND. Q(LPELC+6).NE.0.)THEN
        SCALE_FACTOR = (Q(LPELC+6) - Q(LVCOR+6)) / Q(LPELC+6)
        Q(LPELC+3) = SCALE_FACTOR * Q(LPELC+3)
        Q(LPELC+4) = SCALE_FACTOR * Q(LPELC+4)
        Q(LPELC+5) = SCALE_FACTOR * Q(LPELC+5)
        Q(LPELC+6) = SCALE_FACTOR * Q(LPELC+6)
        Q(LPELC+7) = SCALE_FACTOR * Q(LPELC+7)
        Q(LPELC+14) = SCALE_FACTOR * Q(LPELC+14)
        Q(LPELC+15) = Q(LPELC+15) + (SCALE_FACTOR-1.)*Q(LPELC+17)
        Q(LPELC+16) = Q(LPELC+16) + (SCALE_FACTOR-1.)*Q(LPELC+18)
        Q(LPELC+17) = SCALE_FACTOR * Q(LPELC+17)
        Q(LPELC+18) = SCALE_FACTOR * Q(LPELC+18)
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
