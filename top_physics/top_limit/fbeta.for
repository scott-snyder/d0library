      REAL*8 FUNCTION FBETA(X)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Integrand for integration over BETA, the
C-     expected background contribution. Called by DGAUSS (CERNLIB).
C-
C-   Returned value  : Integrand for BETA integration
C-   Inputs  : X  - Integration variables
C-   Outputs : none
C-   Controls: none
C-
C-   Created  29-JUN-1993   Richard Partridge
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:TOP_LIMDATA.INC'
C
      REAL*8 X
C
      LOGICAL FIRST
C
      INTEGER I
C
      REAL*8 PI
      REAL*8 TERM
      REAL*8 BNORM
      REAL*8 PNORM
      REAL*8 DERF
      REAL*8 XPECT
C
      DATA FIRST/.TRUE./
      DATA PI/3.14159265359D0/
C
C----------------------------------------------------------------------
C
C  Calculate expected number of events and normaliztion for P(NTOT)
C
      BETA = X
      XPECT = BETA + ALPHA*SIGMA
      PNORM = 0.
      TERM = DEXP(-BETA)
      DO I=1,NTOT+1
        PNORM = PNORM + TERM
        TERM = TERM*BETA/DFLOAT(I)
      ENDDO
C
C  Calculate integrand
C
      FBETA = ALPHA * XPECT**NTOT * DEXP(-XPECT) / (FACTORIAL * PNORM)
C
C  See if we need to weight the integrand with a Gaussian for BETA
C
      IF (DBETA.GT.0.) THEN
C
C  Calculate probability normalization for P(BETA)
C
        IF (FIRST) THEN
          BNORM = DSQRT(PI/2.D0) * DBETA *
     &      (DERF((BETA_MAX-BETA0)/(DSQRT(2.D0)*DBETA)) +
     &       DERF((BETA0-BETA_MIN)/(DSQRT(2.D0)*DBETA)))
          FIRST = .FALSE.
        ENDIF
C
C  Weight integrand with a Gaussian distribution in BETA
C
        FBETA = FBETA * DEXP(-(BETA-BETA0)**2/(2.*DBETA**2)) / BNORM
      ENDIF
C
  999 RETURN
      END
