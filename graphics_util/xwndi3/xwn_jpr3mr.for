      SUBROUTINE JPR3MR(X,Y,Z,N)
      REAL X(*),Y(*),Z(*)
      DO I=1,N
        CALL JP3MRK(X(I),Y(I),Z(I))
      ENDDO
      END
