      INTEGER FUNCTION GZEIGN()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to EIGN bank
C-
C-   Returned value  : Link to 1st element of EIGN linear structure
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  20-DEC-1990 15:07:37.26  Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZEIGN.LINK/LIST'
      INCLUDE 'D0$INC:ZHMATRIX.INC'
C----------------------------------------------------------------------
      GZEIGN = LEIGN
C
  999 RETURN
      END
