      REAL FUNCTION DPHISEG(X1,Y1,PHI1,PHE1,X2,Y2,PHI2,PHE2)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : compute (PHI1-PHI)**2/ERR1**2 + (PHI-PHI2)/ERR2**2
C-           where PHI is the angle between the points (x2,y2) and (x1,y1)
C-
C-   Returned value  : 
C-   Inputs  : X1,Y1,PHI1,PHE1 = COG POSITION, PHI AND SIGMA_PHI FOR OUTER SEG
C-             X2,Y2,PHI2,PHE2 = COG POSITION, PHI AND SIGMA_PHI FOR INNER SEG
C-   Outputs : 
C-   Controls: 
C-
C-   Created   3-SEP-1993   Ed Oltman
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PI.DEF'
      REAL X1,Y1,PHI1,PHE1,X2,Y2,PHI2,PHE2,TOLDIS
      REAL DPHI,DPHI1,DPHI2,PHISEG
C----------------------------------------------------------------------
      PHISEG = ATAN2(Y1-Y2,X1-X2)
      IF (PHISEG .LT. 0.) PHISEG = PHISEG + TWOPI
      DPHI1 = ABS(PHISEG-PHI1)
      IF (DPHI1 .GT. 3.) DPHI1 = DPHI1 -TWOPI
      DPHI2 = ABS(PHISEG-PHI2)
      IF (DPHI2 .GT. 3.) DPHI2 = DPHI2 -TWOPI
      DPHI = (DPHI1/PHE1)**2 + (DPHI2/PHE2)**2
      DPHISEG = DPHI
  999 RETURN
      END
