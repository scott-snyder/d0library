      SUBROUTINE GET_CART(POL,CART,MASS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : CONVERTS A POLAR VECTOR INTO A 4 VECTOR
C-   OF MAS MASS
C-
C-   Inputs  : POL(1)=ENERGY,POL(2)=ETA,POL(3)=PHI
C-   Outputs : CART(1:4) = 4 VECTOR +ET,ETA,PHI
C-   Controls:
C-
C-   Created  22-FEB-1994   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      DOUBLE PRECISION    POL(*),CART(*),MASS
      DOUBLE PRECISION    ETA,PHI,THETA,CTHETA,STHETA,PMOM
C----------------------------------------------------------------------
      ETA = POL(2)
      PHI = POL(3)
      THETA = 2.0*ATAN(EXP(-ETA))
      CTHETA = COS(THETA)
      STHETA = SIN(THETA)
      PMOM = SQRT(POL(1)**2-MASS**2)
      CART(1) = PMOM*STHETA*COS(PHI)
      CART(2) = PMOM*STHETA*SIN(PHI)
      CART(3) = PMOM*CTHETA
      CART(4) = POL(1)
      CART(5) = SQRT(CART(1)**2+CART(2)**2)
      CART(6) = ETA
      CART(7) = PHI
C
  999 RETURN
      END
