C DEC/CMS REPLACEMENT HISTORY, Element JDBASE.FOR
C *1     3-JUN-1992 13:26:20 LUPE "Release of DI3GL after split"
C DEC/CMS REPLACEMENT HISTORY, Element JDBASE.FOR
      SUBROUTINE JDBASE(X,Y,Z)
      INCLUDE 'D0$GRAPHICS_UTIL$SGIDI3:DI3GL.INC'
      XBASED=X
      YBASED=Y
      ZBASED=Z
      CALL JBASE(X,Y,Z)
      END