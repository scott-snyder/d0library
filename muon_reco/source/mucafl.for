      SUBROUTINE MUCAFL(ITR,LMUCA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To calculate and fill MUCA bank
C-
C-   Inputs  :  ITR      : muon track number
C-
C-   Outputs :  LMUCA    : MUCA bank address
C-   Controls:
C-
C-   Created  03-APR-1991   SHAHRIAR ABACHI
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ITR, LMUCA
C
      INTEGER NR, LMUON, I, K
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:MUPHYS.INC'
      INCLUDE 'D0$INC:MCACEL.INC'
C
      CALL BKMUCA(NCELL,ITR,LMUCA)
C---
      IQ(LMUCA + 1) = 1
      IQ(LMUCA + 2) = 5
      IQ(LMUCA + 3) = NCELL
C
      NR = 5
      K = 1
      DO I = 1,NCELL
        IQ(LMUCA + 3 + K) = IETAC(I)
        IQ(LMUCA + 4 + K) = IPHIC(I)
        IQ(LMUCA + 5 + K) = LAYERC(I)
        Q(LMUCA + 6 + K)  = DECELL(I)
        Q(LMUCA + 7 + K)  = TLEN(I)
        K = K + NR
      ENDDO
C
  999 RETURN
      END
