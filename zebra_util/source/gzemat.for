      INTEGER FUNCTION GZEMAT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to EMAT bank
C-
C-   Returned value  : Link to 1st element of EMAT linear structure
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  20-DEC-1990 15:07:37.26  Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZEMAT.LINK/LIST'
      INCLUDE 'D0$INC:ZHMATRIX.INC'
C----------------------------------------------------------------------
      GZEMAT = LEMAT
C
  999 RETURN
      END
