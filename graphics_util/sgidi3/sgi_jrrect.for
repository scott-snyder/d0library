C DEC/CMS REPLACEMENT HISTORY, Element JRRECT.FOR
C *1     3-JUN-1992 14:42:48 LUPE "Release of DI3GL after split"
C DEC/CMS REPLACEMENT HISTORY, Element JRRECT.FOR
      SUBROUTINE JRRECT(X,Y)
      INCLUDE 'D0$GRAPHICS_UTIL$SGIDI3:DI3GL.INC'
      REAL X,Y,X0,Y0
      X0=VPOSN(1)
      Y0=VPOSN(2)
      CALL JRECT(X0,Y0,X+X0,Y+Y0)
      VPOSN(1)=X0
      VPOSN(2)=Y0
      END
