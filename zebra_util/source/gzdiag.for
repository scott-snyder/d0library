      INTEGER FUNCTION GZDIAG()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to DIAG bank
C-
C-   Returned value  : Link to 1st element of DIAG linear structure
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  20-DEC-1990 15:07:37.26  Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZDIAG.LINK/LIST'
      INCLUDE 'D0$INC:ZHMATRIX.INC'
C----------------------------------------------------------------------
      GZDIAG = LDIAG
C
  999 RETURN
      END
