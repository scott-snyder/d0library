      SUBROUTINE JPRMRK(X,Y,N)
      REAL X(*),Y(*)
      DO I=1,N
        CALL JRMARK(X(I),Y(I))
      ENDDO
      END
