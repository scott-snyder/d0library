      FUNCTION SQQRT(X)
      IMPLICIT NONE

      REAL*8 SQQRT,X

      IF (X.GE.0.) THEN
        SQQRT=SQRT(X)
      ELSE
        SQQRT=-SQRT(-X)
C           write(*,*) 'Bad sqqrt passed.'
      ENDIF

      RETURN
      END
