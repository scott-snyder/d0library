      REAL FUNCTION DOUBLE_GAUSSIAN(P,X)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : RETURNS A DOUBLE GAUSSIAN 
C-
C-   Returned value  : VALUE OF DOUBLE GAUSSIAN.
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   7-MAR-1994   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    P(6),X,G1,G2
C----------------------------------------------------------------------
      G1 = P(1)*EXP(-0.5*((X-P(2))/P(3))**2)
      G2 = P(4)*EXP(-0.5*((X-P(5))/P(6))**2)
      DOUBLE_GAUSSIAN = G1+G2
  999 RETURN
      END
