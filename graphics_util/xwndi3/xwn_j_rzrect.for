      SUBROUTINE J_RZRECT(X1,Y1,X2,Y2)
      CALL J_RZLINE2(X1,Y1,X1,Y2)
      CALL J_RZLINE2(X1,Y2,X2,Y2)
      CALL J_RZLINE2(X2,Y2,X2,Y1)
      CALL J_RZLINE2(X2,Y1,X1,Y1)
      END