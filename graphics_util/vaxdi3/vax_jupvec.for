      SUBROUTINE JUPVEC(X,Y,Z)
      REAL X,Y,Z,R2
      INCLUDE 'D0$INC:DI3INC.INC'
      R2=X*X+Y*Y+Z*Z
      IF(R2.LE.0)THEN
        WRITE(JUNIT,*)'JUPVEC CALLED WITH ZERO VECTOR'
        RETURN
      ENDIF
      R2=1./SQRT(R2)
      VUPX=X*R2
      VUPY=Y*R2
      VUPZ=Z*R2
C  RECALC VIEW VECTORS
      CALL J_VUVECTS
      END
