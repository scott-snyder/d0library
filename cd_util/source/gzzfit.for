      INTEGER FUNCTION GZZFIT(IZTRK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank ZFIT
C-
C-   Returned value  : pointer to Zebra bank ZFIT
C-
C-   Created  19-MAR-1990   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZZTRK, IZTRK, LZTRK
      INCLUDE 'D0$LINKS:IZZFIT.LINK'
      INCLUDE 'D0$INC:ZEBCOM.INC'
C----------------------------------------------------------------------
      GZZFIT = 0
      LZTRK = GZZTRK(IZTRK)
      IF (LZTRK .GT. 0) GZZFIT = LQ(LZTRK - IZZFIT)
C----------------------------------------------------------------------
  999 RETURN
      END
