      INTEGER FUNCTION GZAVER()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to AVER bank
C-
C-   Returned value  : Link to 1st element of AVER linear structure
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  20-DEC-1990 15:07:37.26  Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZAVER.LINK/LIST'
      INCLUDE 'D0$INC:ZHMATRIX.INC'
C----------------------------------------------------------------------
      GZAVER = LAVER
C
  999 RETURN
      END
