      SUBROUTINE JP3MRK(X,Y,Z,N)
      INTEGER I,N
      REAL X(2),Y(2),Z(2)
      DO 10 I=1,N
        CALL J3MARK(X(I),Y(I),Z(I))
   10 CONTINUE
      END
