      DOUBLE PRECISION FUNCTION XYGAUZ(X)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Gaussian for Z-coordinate with 2 sigmas
C-                         (function for HFIT)
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
      IF (X.LE.PAR(2)) THEN
        XYGAUZ = PAR(1)*EXP(-0.5*(X-PAR(2))**2/PAR(3)**2)
      ELSE
        XYGAUZ = PAR(1)*EXP(-0.5*(X-PAR(2))**2/PAR(4)**2)
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
