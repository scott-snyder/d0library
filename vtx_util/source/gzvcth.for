      FUNCTION GZVCTH()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  25-FEB-1994   Al Clark
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZVCTH.LINK'
C
      INTEGER GZVCTH
C
      INTEGER LVTRH, GZVTRH
C----------------------------------------------------------------------
      GZVCTH = 0
      LVTRH = GZVTRH()
      IF (LVTRH .GT. 0 )  GZVCTH = LQ(LVTRH-IZVCTH)
  999 RETURN
      END
