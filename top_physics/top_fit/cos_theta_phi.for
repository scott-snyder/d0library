      SUBROUTINE COS_THETA_PHI(P,CTH,PHI)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : GIVEN 3 VECTOR , RETURNS COS THETA AND PHI
C-   OF VECTOR. PHI = 0-2PI
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  14-FEB-1994   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PI.DEF'
      DOUBLE PRECISION    P(*),CTH,PHI
      DOUBLE PRECISION    ET,PP
C----------------------------------------------------------------------
      ET = P(1)**2 + P(2)**2
      PP = ET + P(3)**2
      PP = SQRT(PP)
      CTH = (P(3)/PP)
      PHI = ATAN2(P(2),P(1))
C
      IF ( PHI.LT.0.0 ) THEN
        PHI = PHI + TWOPI
      ENDIF
C
  999 RETURN
      END
