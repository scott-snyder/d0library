      SUBROUTINE TK_DRASCR(X,Y)
C  Screen coordinates
      COMMON/J_TKTEM/X1,Y1,X2,Y2
      X2=X
      Y2=Y
      CALL TK_LINE2(X1,Y1,X2,Y2)
      X1=X2
      Y1=Y2
      END
