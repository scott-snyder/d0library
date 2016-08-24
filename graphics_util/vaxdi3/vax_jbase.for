      SUBROUTINE JBASE(X,Y,Z)
C  BASE AND PLANE VECTORS FOR TEXT
      REAL R2,X,Y,Z
      INCLUDE 'D0$INC:DI3INC.INC'
      R2 = X*X + Y*Y + Z*Z
      IF(R2.LE.0)THEN
        WRITE(JUNIT,*)'JBASE CALLED WITH ZERO VECTOR'
        RETURN
      ENDIF
      XBASE=X
      YBASE=Y
      ZBASE=Z
      R=SQRT(R2)
      XBASEN=XBASE/R
      YBASEN=YBASE/R
      ZBASEN=ZBASE/R
      IF(.NOT.PUTS)RETURN
      IF(PUTSUP)RETURN
      CALL J_PUTSG(IJBASE,X)
      CALL J_PUTSG(IJBASE,Y)
      CALL J_PUTSG(-IJBASE,Z)
      END