      SUBROUTINE SET_MAX_MOMENTUM(P4,PMAX)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Normalizes four vector to energy PMAX
C-                         TO handle muon mismeasures
C-
C-   Inputs  : P4 4 vector of particle.
C-   Outputs : 
C-   Controls: 
C-
C-   Created  31-AUG-1993   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      DOUBLE PRECISION P4(*)
      REAL    PMAX
      INTEGER I
      REAL    FACT
C----------------------------------------------------------------------
      FACT = P4(4)/PMAX
      IF ( FACT.GT.1.0 ) THEN
        DO I = 1 , 4
          P4(I) = P4(I)/FACT
        ENDDO
      ENDIF
  999 RETURN
      END
