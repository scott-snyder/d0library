C DEC/CMS REPLACEMENT HISTORY, Element JDSIZE.FOR
C *1     3-JUN-1992 13:48:53 LUPE "Release of DI3GL after split"
C DEC/CMS REPLACEMENT HISTORY, Element JDSIZE.FOR
      SUBROUTINE JDSIZE(X,Y)
      INCLUDE 'D0$GRAPHICS_UTIL$SGIDI3:DI3GL.INC'
      XSIZED=X
      YSIZED=Y
      CALL JSIZE(X,Y)
      END
