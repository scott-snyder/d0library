      REAL FUNCTION DIFF_PHI(PHI1,PHI2)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : RETURNS DIFFERENCE BETWEEN PHI1 AND PHI2 IN RADIANS
C-
C-   Inputs  : PHI1, PHI2
C-   Outputs :
C-   Controls:
C-
C-   Created  13-OCT-1989   Chip Stewart
C-   Updated  27-MAR-1995   Feng Wen, Chip Stewart - fixed 0 to HALFPI  
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PI.DEF'
      REAL    PHI1,PHI2,X1,Y1
C----------------------------------------------------------------------
      X1 = COS(PHI2-PHI1)
      Y1 = SIN(PHI2-PHI1)
      DIFF_PHI = HALFPI
      IF(X1.EQ.0.0) RETURN
      DIFF_PHI = ATAN2(Y1,X1)
  999 RETURN
      END
