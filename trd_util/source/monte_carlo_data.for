      LOGICAL FUNCTION MONTE_CARLO_DATA

C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Returned value  :
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  20-DEC-1994
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:zebcom.INC'
C----------------------------------------------------------------------
      MONTE_CARLO_DATA=.FALSE.
      IF (IQ(LHEAD+1) .GT. 1000)  MONTE_CARLO_DATA= .TRUE.
  999 RETURN
      END
