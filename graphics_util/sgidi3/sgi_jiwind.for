C DEC/CMS REPLACEMENT HISTORY, Element JIWIND.FOR
C *1     3-JUN-1992 14:02:36 LUPE "Release of DI3GL after split"
C DEC/CMS REPLACEMENT HISTORY, Element JIWIND.FOR
      SUBROUTINE JIWIND(XV,YV,ZV)
C  RETURN THE CORNERS OF THE VIRTUAL WINDOW IN WORLD COORDINATES
C  (NOT EXACTLY RIGHT, BUT WILL WORK IN 2D PLOTS WHEN NO JVPORT
C  SELECTED).
      REAL XV(4),YV(4),ZV(4)
      INCLUDE 'D0$GRAPHICS_UTIL$SGIDI3:DI3GL.INC'
      XV(1)=UMIN
      XV(2)=UMIN
      XV(3)=UMAX
      XV(4)=UMAX
      YV(1)=VMIN
      YV(2)=VMAX
      YV(3)=VMAX
      YV(4)=VMIN
      END
