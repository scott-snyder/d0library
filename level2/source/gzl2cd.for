      FUNCTION GZL2CD()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  30-MAR-1992   D. Claes
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZL2CD.LINK'
      INTEGER GZL2CD
      INTEGER GZFRES,LSUP1
C----------------------------------------------------------------------
      GZL2CD = 0
      LSUP1 = GZFRES()
      IF (LSUP1 .GT. 0)  GZL2CD = LQ(LSUP1 - IZL2CD)  
  999 RETURN
      END
