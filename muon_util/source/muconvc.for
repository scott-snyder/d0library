      SUBROUTINE MUCONVC(ETA_PHYS,PHI,PC,THE_D,ETA_D)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Conversion of physics coordinates to detector
C-                         coordinates for muon system.
C-
C-   Inputs  : ETA_PHYS    Physics eta
C-             PHI         phi
C-             PC          A point where track vector defined
C-
C-   Outputs : THE_D     Detector theta
C-             ETA_D     Detector eta
C-
C-   Controls:
C-
C-   Created   3-JUL-1992   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL ETA_PHYS,PHI,PC(3),THE_D,ETA_D
      INCLUDE 'D0$INC:PI.DEF'
      REAL DIRC(3),XC(3),ALPHA,THE_PHYS
      REAL C(3),R
      INTEGER I,IHIT
      REAL DD,LL,BS,VV,LX,XX
C
      THE_PHYS = 2.0 * ATAN(EXP(-ETA_PHYS))
      DIRC(1) = SIN(THE_PHYS) * COS(PHI)
      DIRC(2) = SIN(THE_PHYS) * SIN(PHI)
      DIRC(3) = COS(THE_PHYS)
C
      CALL MUPLNZX(0.0,PC,DIRC,C,IHIT)
      IF(IHIT .EQ. 0) CALL MUPLNYZ(0.0,PC,DIRC,C,IHIT)
      CALL MUPLN(C,DIRC,XC)
C
      DD = SQRT(XC(1)**2 + XC(2)**2)
      ALPHA = HALFPI - THE_PHYS
      LL = DD / COS(ALPHA)
      BS = SQRT(LL**2 - DD**2)
      VV = SQRT(C(1)**2 + C(2)**2 + C(3)**2)
      LX = VV + BS
      XX = SQRT(DD**2 + LX**2)
C
      THE_D = ACOS(LX / XX)
      ETA_D = -ALOG(TAN(THE_D/2.0))
C
C----------------------------------------------------------------------
  999 RETURN
      END
