      REAL FUNCTION DRCOS(PHI1,THETA1,PHI2,THETA2)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns cosine of the angle between two 
C-                         vectors with directions PHI1,THETA1,PHI2,THETA2
C-
C-   Returned value  :    dot product
C-   Inputs  :         PHI1:   Phi of track 1
C-                     THETA1: Theta of track 1
C-                     PHI2:   Phi of track 2
C-                     THETA2: Theta of track 2
C-   Outputs : DRCOS 
C-   Controls: None
C-
C-   Created   3-MAY-1994   Norman A. Graf
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL PHI1,THETA1,PHI2,THETA2
      REAL X,Y,Z,THETA,PHI
C
      X(THETA,PHI) = SIN(THETA)*COS(PHI)
      Y(THETA,PHI) = SIN(THETA)*SIN(PHI)
      Z(THETA,PHI) = COS(THETA)
C----------------------------------------------------------------------
      DRCOS = X(PHI1,THETA1)*X(PHI2,THETA2) +
     &        Y(PHI1,THETA1)*Y(PHI2,THETA2) +
     &        Z(PHI1,THETA1)*Z(PHI2,THETA2)
  999 RETURN
      END

