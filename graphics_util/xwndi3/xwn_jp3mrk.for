      SUBROUTINE JP3MRK(X,Y,Z,N)
      INTEGER I,N
      REAL X(*),Y(*),Z(*)
      DO I=1,N
        CALL J3MARK(X(I),Y(I),Z(I))
      ENDDO
      END
