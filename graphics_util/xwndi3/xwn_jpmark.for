      SUBROUTINE JPMARK(X,Y,N)
      REAL X(*),Y(*)
      INTEGER I,N
      DO I=1,N
        CALL JMARK(X(I),Y(I))
      ENDDO
      END
