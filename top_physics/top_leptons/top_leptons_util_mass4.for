      FUNCTION TOP_LEPTONS_UTIL_MASS4(VECTOR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Compute Mass of 4-vector
C-
C-   Returned value  : Mass
C-
C-   Inputs  : Particle 4-vector
C-
C-   Outputs : None
C-
C-   Controls: None
C-
C-   Created   4-SEP-1991   Stephen J. Wimpenny
C-   Modified 17-Mar-1993   Name changed for library compatibility
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL VECTOR,PSQ,MSQ,TOP_LEPTONS_UTIL_MASS4
      DIMENSION VECTOR(4)
C
      PSQ=VECTOR(1)**2+VECTOR(2)**2+VECTOR(3)**2
      MSQ=VECTOR(4)**2-PSQ
      IF(MSQ.GT.0.) THEN
        TOP_LEPTONS_UTIL_MASS4=SQRT(MSQ)
      ELSE
        TOP_LEPTONS_UTIL_MASS4=0.0
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
