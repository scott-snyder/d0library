      DOUBLE PRECISION FUNCTION XYVSUG(X)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : sum of two Gaussians (for HFIT)
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   5-OCT-1992   Alexandre Zinchenko
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:XYVCOM.INC'
      REAL X
C
      XYVSUG = PAR(1)*EXP(-0.5*(X-PAR(2))**2/PAR(3)**2)
      XYVSUG = XYVSUG + PAR(4)*EXP(-0.5*(X-PAR(5))**2/PAR(6)**2)
C----------------------------------------------------------------------
  999 RETURN
      END
