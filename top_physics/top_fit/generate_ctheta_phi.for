      SUBROUTINE GENERATE_CTHETA_PHI(COST,SINT,PHI)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : GENERATES AN ISOTROPIC COS THETA , PHI
C-                           DISTRIBUTION
C-
C-   Inputs  :
C-   Outputs : COST,SINT,PHI = COS THETA , SIN THETA AND PHI(IN RADIANS)
C-   Controls:
C-
C-   Created  20-FEB-1994   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PI.DEF'
      DOUBLE PRECISION    RNDM
      DOUBLE PRECISION    COST,SINT,PHI
C----------------------------------------------------------------------
      COST = 2.0*RNDM(0)-1.
      SINT = SQRT(1.-COST**2)
      PHI = RNDM(0)*TWOPI
  999 RETURN
      END
