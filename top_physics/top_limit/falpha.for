      REAL*8 FUNCTION FALPHA(X)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Integrand for integration over ALPHA, the
C-     cross section multiplier. Called by DGAUSS (CERNLIB).
C-
C-   Returned value  : Integrand for ALPHA integration
C-   Inputs  : X  - Integration variables
C-   Outputs : none
C-   Controls: none
C-
C-   Created  29-JUN-1993   Richard Partridge
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      EXTERNAL FBETA
C
      INCLUDE 'D0$INC:TOP_LIMDATA.INC'
C
      REAL*8 X
C
      LOGICAL FIRST
C
      REAL*8 PI
      REAL*8 ANORM
      REAL*8 DERF
      REAL*8 DGAUSS2
      REAL*8 FBETA
C
      DATA FIRST/.TRUE./
      DATA PI/3.14159265359D0/
C
C----------------------------------------------------------------------
C
C  Calculate integrand
C
      ALPHA = X
      IF (DBETA.GT.0.) THEN
        FALPHA = DGAUSS2(FBETA,BETA_MIN,BETA_MAX,EPS)
      ELSE
        FALPHA = FBETA(BETA0)
      ENDIF
C
C  See if we need to weight the integrand with a Gaussian for ALPHA
C
      IF (DALPHA.GT.0.) THEN
C
C  Calculate probability normalization for P(ALPHA)
C
        IF (FIRST) THEN
          ANORM = DSQRT(PI/2.D0) * DALPHA *
     &      (DERF((ALPHA_MAX-ALPHA0)/(DSQRT(2.D0)*DALPHA)) +
     &       DERF((ALPHA0-ALPHA_MIN)/(DSQRT(2.D0)*DALPHA)))
          FIRST = .FALSE.
        ENDIF
C
C  Weight integrand with a Gaussian distribution in ALPHA
C
        FALPHA = FALPHA * DEXP(-(ALPHA-ALPHA0)**2/(2.*DALPHA**2))/ANORM
      ENDIF
C
  999 RETURN
      END
