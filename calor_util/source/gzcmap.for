      INTEGER FUNCTION GZCMAP()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to CMAP bank
C-
C-   Returned value  : Address of CMAP bank
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   9-JAN-1990  Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZLINKC.INC'
C----------------------------------------------------------------------
      GZCMAP=LCMAP
  999 RETURN
      END
