      REAL FUNCTION XYVGA2(X,Y)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : sum of two 2-dimensional Gaussians
C-                         (for HFIT)
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   5-OCT-1992   Alexandre Zinchenko
C-   Updated   2-APR-1993   A.Zinchenko - change to one 2-dim. Gaussian
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:XYVCOM.INC'
      INTEGER I, J
      REAL X, Y, AM, ZSIG, P(2)
      EQUIVALENCE (PAR(7), P(1))
C
cc      IF (PAR(3).LT.PAR(6)) THEN
        I = 2
        J = 5
cc      ELSE
cc        I = 5
cc        J = 2
cc      ENDIF
      AM = PAR(I) + P(1)*(Y-ZMEAN)
      IF (Y.LE.ZMEAN) THEN
        ZSIG = ZSIG1
      ELSE
        ZSIG = ZSIG2
      ENDIF
C      XYVGA2 = PAR(I-1)*EXP(-0.5*(X-AM)**2/PAR(I+1)**2)
C      XYVGA2 = XYVGA2*PAR(8)*EXP(-0.5*(Y-ZMEAN)**2/ZSIG**2)
      XYVGA2 = PAR(I-1)*EXP(-0.5*(X-AM)**2/SNGL(PAR(I+1)**2))
      XYVGA2 = XYVGA2*P(2)*EXP(-0.5*(Y-ZMEAN)**2/ZSIG**2)
C----------------------------------------------------------------------
  999 RETURN
      END

