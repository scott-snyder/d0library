C DEC/CMS REPLACEMENT HISTORY, Element JDEVVP.FOR
C *1     3-JUN-1992 13:40:28 LUPE "Release of DI3GL after split"
C DEC/CMS REPLACEMENT HISTORY, Element JDEVVP.FOR
      SUBROUTINE JDEVVP(IDEV,DXMIN,DXMAX,DYMIN,DYMAX)
      INCLUDE 'D0$GRAPHICS_UTIL$SGIDI3:DI3GL.INC'
      IF(IDEBUG.GT.5)TYPE 2222,IDEV,DXMIN,DXMAX,DYMIN,DYMAX
 2222 FORMAT(' JDEVVP:',I,4F)
      END
