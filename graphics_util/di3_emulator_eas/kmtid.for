      SUBROUTINE KMTID(MAT)
C
C   Generate an identity matrix.
C
      IMPLICIT NONE
      REAL MAT(4,4)
      INTEGER I, J

      DO 10 I=1,4
         DO 10 J=1,4
            IF (I.EQ.J) THEN
               MAT(I,J) = 1.0
            ELSE
               MAT(I,J) = 0.0
            ENDIF
   10 CONTINUE
      RETURN
      END
