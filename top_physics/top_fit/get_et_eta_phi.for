      SUBROUTINE GET_ET_ETA_PHI(P)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : GIVEN A 4 VECTOR WORKS OUT ET, ETA AND PHI
C-
C-   Inputs  : P(4)
C-   Outputs : P(5_7)=ET,ETA,PHI
C-   Controls:
C-
C-   Created  15-FEB-1994   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PI.DEF'
      DOUBLE PRECISION    P(*)
      DOUBLE PRECISION    ET,ETA,PHI
      DOUBLE PRECISION    THETA
C----------------------------------------------------------------------
      ET = DSQRT(P(1)**2+P(2)**2)
      PHI = ATAN2(P(2),P(1))
      IF ( PHI.LT.0.0 ) THEN
        PHI = PHI + TWOPI
      ENDIF
      THETA = ATAN2(ET,P(3))
      ETA = -DLOG(DTAN(THETA/2.))
      P(5) = ET
      P(6) = ETA
      P(7) = PHI
  999 RETURN
      END
