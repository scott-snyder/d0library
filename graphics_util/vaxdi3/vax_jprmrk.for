      SUBROUTINE JPRMRK(X,Y,N)
      INTEGER I,N
      REAL X(2),Y(2)
      DO 10 I=1,N
        CALL JRMARK(X(I),Y(I))
   10 CONTINUE
      END
