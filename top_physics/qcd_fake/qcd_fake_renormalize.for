      SUBROUTINE QCD_FAKE_RENORMALIZE(VECT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : RE-CALCULATES ENERGY AND ET USING THE THREE
C-   COMPONENTS  AND ASSUMING ZERO MASS.
C-
C-   Inputs  : VECT = 7 VECTOR OF PARTICLE. ENERGY,ET,ETA,PHI
C-   Outputs : 
C-   Controls: 
C-
C-   Created  30-MAR-1994   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      DOUBLE PRECISION    VECT(*),E,ET,THETA,ETA,PHI
      INCLUDE 'D0$INC:PI.DEF'
C----------------------------------------------------------------------
      ET = VECT(1)**2 + VECT(2)**2
      E = SQRT(VECT(3)**2+ET)
      ET = SQRT(ET)
      VECT(4) = E
      VECT(5) = ET
      THETA = ATAN2(ET,VECT(3))
      ETA = -DLOG(DTAN(THETA/2.))
      PHI = ATAN2(VECT(2),VECT(1))
      IF ( PHI.LT.0.0 ) THEN
        PHI = PHI + TWOPI
      ENDIF
      VECT(6) = ETA
      VECT(7) = PHI
  999 RETURN
      END
