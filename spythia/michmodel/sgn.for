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
