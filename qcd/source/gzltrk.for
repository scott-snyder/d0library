      FUNCTION GZLTRK
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Returned value  :
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   8-JUL-1993   Brent J. May
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZLTRK, GZMDST, LMDST
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZLTRK.LINK'
C----------------------------------------------------------------------
      GZLTRK = 0
      LMDST = GZMDST()
      IF ( LMDST .GT. 0 ) GZLTRK = LQ(LMDST-IZLTRK)
  999 RETURN
      END
