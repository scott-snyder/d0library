C DEC/CMS REPLACEMENT HISTORY, Element JRMARK.FOR
C *1     3-JUN-1992 14:38:20 LUPE "Release of DI3GL after split"
C DEC/CMS REPLACEMENT HISTORY, Element JRMARK.FOR
      SUBROUTINE JRMARK(X,Y)
      REAL X,Y
      INCLUDE 'D0$GRAPHICS_UTIL$SGIDI3:DI3GL.INC'
      XNOW=X+VPOSN(1)
      YNOW=Y+VPOSN(2)
      CALL JMARK(XNOW,YNOW)
      END