      LOGICAL FUNCTION MONTE_CARLO_DATA()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : IF TRUE, DATA IS MONTE CARLO IN CHARACTER
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  23-APR-1993   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C----------------------------------------------------------------------
      MONTE_CARLO_DATA=.FALSE.
      IF ( IQ(LHEAD+1).GE.1005 ) THEN
        MONTE_CARLO_DATA = .TRUE.
      ENDIF
  999 RETURN
      END
