      INTEGER FUNCTION GZCDCH()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank CDCH 
C-
C-   Returned value  : pointer to Zebra bank CDCH 
C-
C-   Created  19-APR-1989   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZHITS, LHITS
      INCLUDE 'D0$LINKS:IZCDCH.LINK'
      INCLUDE 'D0$INC:ZEBCOM.INC'
C----------------------------------------------------------------------
      GZCDCH = 0
      LHITS = GZHITS()
      IF (LHITS .GT. 0) GZCDCH = LQ(LHITS - IZCDCH)
C----------------------------------------------------------------------
  999 RETURN
      END
