      FUNCTION GZL2DRFT()
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
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER GZL2DRFT
      INTEGER GZSL2H,LSL2H
C----------------------------------------------------------------------
      GZL2DRFT = 0
      LSL2H = GZSL2H()
      IF (LSL2H .GT. 0)  GZL2DRFT = LC( LC( LC(LSL2H-9) -4)  -3)
C                                             L2CDC    DGEH  DRFT
  999 RETURN
      END
