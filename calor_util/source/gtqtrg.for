      SUBROUTINE GTQTRG(IVER,L0_ZVERT,NUM,ET,ETA,PHI)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  17-JUN-1992   Andrew J. Milder
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER NUM,ETA(*),PHI(*),IVER,LQTRG,GZQTRG,I
      REAL L0_ZVERT,ET(*)
C----------------------------------------------------------------------
      LQTRG = GZQTRG()
      IVER = IQ(LQTRG+1)
      NUM = IQ(LQTRG+2)
      L0_ZVERT = Q(LQTRG+5)
      DO I = 1, NUM
        ETA(I) = IQ(LQTRG + 6 + 3*(I-1))
        PHI(I) = IQ(LQTRG + 7 + 3*(I-1))
        ET(I) =   Q(LQTRG + 8 + 3*(I-1))
      ENDDO
C
  999 RETURN
      END
