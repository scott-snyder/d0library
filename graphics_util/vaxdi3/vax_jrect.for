      SUBROUTINE JRECT(X0,Y0,X1,Y1)
      INCLUDE 'D0$INC:DI3INC.INC'
      REAL XXX(4),YYY(4)
      XXX(1)=X0
      YYY(1)=Y0
      XXX(2)=X0
      YYY(2)=Y1
      XXX(3)=X1
      YYY(3)=Y1
      XXX(4)=X1
      YYY(4)=Y0
      CALL JPOLGN(XXX,YYY,4)
      END
