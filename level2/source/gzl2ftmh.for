      FUNCTION GZL2FTMH()
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
C-   Updated  30-MAY-1992   Y.C. LIU  For FDC  in L2
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INTEGER GZL2FTMH
      INTEGER GZSL2H,LSL2H
C----------------------------------------------------------------------
      GZL2FTMH = 0
      LSL2H = GZSL2H()
      IF (LSL2H .GT. 0)  GZL2FTMH = LC( LC(LSL2H-13) -3)
C                                         L2FDC     FTMH 
  999 RETURN
      END
