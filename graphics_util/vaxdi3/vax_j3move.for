      SUBROUTINE J3MOVE(X,Y,Z)
      REAL X,Y,Z
      INCLUDE 'D0$INC:DI3INC.INC'
      XPOSN=X
      YPOSN=Y
      ZPOSN=Z
      IF(.NOT.PUTS)RETURN
      IF(PUTSUP)RETURN
      CALL J_PUTSG(I3MOVE,X)
      CALL J_PUTSG(I3MOVE,Y)
      CALL J_PUTSG(-I3MOVE,Z)
      END
