      FUNCTION GZL2FD()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  30-MAR-1992   YI-CHENG LIU
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZL2FD.LINK'
      INTEGER GZL2FD
      INTEGER GZFRES,LSUP1
C----------------------------------------------------------------------
      GZL2FD = 0
      LSUP1 = GZFRES()
      IF (LSUP1 .GT. 0)  GZL2FD = LQ(LSUP1 - IZL2FD)  
  999 RETURN
      END
