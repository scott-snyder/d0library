      FUNCTION GZL2CR()
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
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZL2CR.LINK'
      INTEGER GZL2CR
      INTEGER GZSL2H,LSL2H
C----------------------------------------------------------------------
      GZL2CR = 0
      LSL2H = GZSL2H()
      IF (LSL2H .GT. 0)  GZL2CR = LC( LSL2H - IZL2CR )
  999 RETURN
      END
