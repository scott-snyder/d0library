      SUBROUTINE J_TRDEV(X,Y,XP,YP)
C  TRANSFORM TO SCREEN COORDINATE SYSTEM.
C  SCALE FACTORS HAVE BEEN SET UP IN J_COORDS
      INCLUDE 'D0$GRAPHICS_UTIL$XWNDI3:XWNEMU.INC'
      XP=(X-VCXMN)*XSCALS+XMARG
      YP=(Y-VCYMN)*YSCALS+YMARG
      END