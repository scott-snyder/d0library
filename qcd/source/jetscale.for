      REAL FUNCTION JETSCALE(ET,ETA)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns corrected jet energy based on test
C-                         beam responses. See D0note #1595 for details.
C-
C-   Inputs  : ET     -  uncorrected jet Et
C-             ETA    -  jet eta
C-   Outputs : JETSCALE - corrected jet Et
C-   Controls: 
C-
C-   Created  24-SEP-1992   Andrew J. Milder
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL ET,A(3),ETA,THETA,ENERGY
      LOGICAL FIRST
      DATA A / -1.666,  0.896,  0.000239 /
      DATA FIRST / .TRUE. /
C----------------------------------------------------------------------
C  
C  QUADRATIC fit used
C  Version 3.0 includes:
C                 i) Pushpa Bhat's latest low-e single particle
C                    responses as of 11/10/92.
C                ii) EM crack response ( negligible effect.)
C               iii) FH crack response ( ~1% effect.)
C
C----------------------------------------------------------------------
      IF (FIRST) THEN
        CALL ERRMSG('JetScale in use','JETSCALE',
     &    'Version 2.1 energy scale correction used','I')
        FIRST = .FALSE.
      ENDIF
C
C Must correct jet energy, not Et
C
      THETA = 2.*ATAN(EXP(-ETA))
      ENERGY = ET/SIN(THETA)
C
      JETSCALE = -A(2)+SQRT(A(2)**2+4.*A(3)*(ENERGY-A(1)))
      JETSCALE = JETSCALE/(2.*A(3)*ENERGY)
      JETSCALE = JETSCALE*SIN(THETA)
C      
  999 RETURN
      END
