C DEC/CMS REPLACEMENT HISTORY, Element JDFONT.FOR
C *1     3-JUN-1992 13:43:04 LUPE "Release of DI3GL after split"
C DEC/CMS REPLACEMENT HISTORY, Element JDFONT.FOR
      SUBROUTINE JDFONT(IDDD)
      INCLUDE 'D0$GRAPHICS_UTIL$SGIDI3:DI3GL.INC'
      IFONDF=IDDD
      CALL JFONT(IDDD)
      END
