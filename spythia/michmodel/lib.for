      FUNCTION SGN(X)
      IMPLICIT NONE
      REAL*8 SGN,X

      IF (X.GE.0.) THEN
        SGN=1.
      ELSE
        SGN=-1.
      ENDIF

      RETURN
      END

Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

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

