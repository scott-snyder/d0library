      SUBROUTINE JCONVW(VX,VY,X,Y,Z)
C  CONVERT VIRTUAL WINDOW TO WORLD COORDINATES
      INCLUDE 'D0$GRAPHICS_UTIL$XWNDI3:XWNEMU.INC'
C  FROM VIEWPORT TO VIEWSPACE
      XP=(VX-VXMIN)/XSCALE+UMIN
      YP=(VY-VYMIN)/YSCALE+VMIN
      X=XVW+XP
      Y=YVW+YP
      Z=0.
      END