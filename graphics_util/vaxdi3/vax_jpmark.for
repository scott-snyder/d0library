      SUBROUTINE JPMARK(X,Y,N)
      REAL X(2),Y(2)
      INTEGER I,N
      DO 10 I=1,N
        CALL JMARK(X(I),Y(I))
   10 CONTINUE
      END
