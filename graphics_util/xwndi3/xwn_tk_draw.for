      SUBROUTINE TK_DRAW(X,Y,Z)
C  World coordinates
      INCLUDE 'D0$GRAPHICS_UTIL$XWNDI3:XWNEMU.INC'
      COMMON/TKTEM/X1,Y1,X2,Y2
      CALL DEV_TRANSF(X,Y,Z,X2,Y2)
      CALL TK_LINE2(X1,Y1,X2,Y2)
      X1=X2
      Y1=Y2
      XPOSN=X
      YPOSN=Y
      ZPOSN=Z
      END
