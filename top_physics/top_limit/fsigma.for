      REAL*8 FUNCTION FSIGMA(X)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Integrand for integration over SIGMA, the
C-     top cross section. Called by DGAUSS (CERNLIB).
C-
C-   Returned value  : Integrand for SIGMA integration
C-   Inputs  : X  - Integration variables
C-   Outputs : none
C-   Controls: none
C-
C-   Created  29-JUN-1993   Richard Partridge
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      EXTERNAL FALPHA
C
      INCLUDE 'D0$INC:TOP_LIMDATA.INC'
C
      REAL*8 X
C
      REAL*8 A,B
      REAL*8 ALPHA_MED
      REAL*8 DGAUSS1
      REAL*8 FALPHA
C
C----------------------------------------------------------------------
C
C  Calculate integrand
C
      SIGMA = X
      IF (DALPHA.GT.0.) THEN
C
C  Estimate the value of alpha for which the remainder of the integral
C  is smaller than EPS by finding the point where the Poisson probability
C  is a factor of EPS smaller than its maximum. Truncate the integration
C  region at this point. This is needed for large values of SIGMA, where
C  a small range of ALPHA near zero contributes to the integral
C
        ALPHA_MED = (DFLOAT(NTOT) - DLOG(EPS)) / SIGMA
        IF ((ALPHA_MED.GT.ALPHA_MIN).AND.(ALPHA_MED.LT.ALPHA_MAX)) THEN
          FSIGMA = DGAUSS1(FALPHA,ALPHA_MIN,ALPHA_MED,EPS)
        ELSE
          FSIGMA = DGAUSS1(FALPHA,ALPHA_MIN,ALPHA_MAX,EPS)
        ENDIF
      ELSE
        FSIGMA = FALPHA(ALPHA0)
      ENDIF
C
  999 RETURN
      END
