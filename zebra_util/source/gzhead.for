      INTEGER FUNCTION GZHEAD()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to HEAD bank
C-
C-   Returned value  : Link to 1st element of HEAD linear structure
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   2-JUL-1990 15:40:44.15  Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C----------------------------------------------------------------------
C
      GZHEAD=LHEAD
C
  999 RETURN
      END
