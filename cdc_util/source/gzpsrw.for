      INTEGER FUNCTION GZPSRW()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  23-OCT-1995   Hailin Li
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZPSRW.LINK'
      INTEGER LPSHT,GZPSHT
C----------------------------------------------------------------------
      GZPSRW = 0
C
      LPSHT = GZPSHT()
      IF (LPSHT .LE. 0 ) RETURN
      GZPSRW = LQ(LPSHT-IZPSRW)
  999 RETURN
      END
