      FUNCTION GZSL2H()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  28-SEP-1990   Richard V. Astur
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZSL2H
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSL2H.LINK'
C----------------------------------------------------------------------
      GZSL2H = 0
      IF (LSTPH .GT. 0)  GZSL2H = LC( LSTPH - IZSL2H )
  999 RETURN
      END
