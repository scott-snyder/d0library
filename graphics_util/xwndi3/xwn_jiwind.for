      SUBROUTINE JIWIND(XV,YV,ZV)
C  RETURN THE CORNERS OF THE VIRTUAL WINDOW IN WORLD COORDINATES
C  (NOT EXACTLY RIGHT, BUT WILL WORK IN 2D PLOTS WHEN NO JVPORT
C  SELECTED).
      REAL XV(4),YV(4),ZV(4)
      INCLUDE 'D0$GRAPHICS_UTIL$XWNDI3:XWNEMU.INC'
      XV(1)=UMIN
      XV(2)=UMIN
      XV(3)=UMAX
      XV(4)=UMAX
      YV(1)=VMIN
      YV(2)=VMAX
      YV(3)=VMAX
      YV(4)=VMIN
      END