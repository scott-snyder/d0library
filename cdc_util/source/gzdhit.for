      FUNCTION GZDHIT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank DHIT
C-
C-   Returned value  : pointer to Zebra bank DHIT
C-
C-   Created  10-JUL-1991   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZDHIT.LINK'
      INTEGER GZDHIT, GZCDCH, LCDCH
C----------------------------------------------------------------------
      GZDHIT = 0
      LCDCH = GZCDCH()
      IF ( LCDCH .GT. 0 ) GZDHIT = LQ(LCDCH - IZDHIT)
  999 RETURN
      END
