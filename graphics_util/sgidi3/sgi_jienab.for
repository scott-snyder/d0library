C DEC/CMS REPLACEMENT HISTORY, Element JIENAB.FOR
C *1     3-JUN-1992 13:58:19 LUPE "Release of DI3GL after split"
C DEC/CMS REPLACEMENT HISTORY, Element JIENAB.FOR
      SUBROUTINE JIENAB(DSPDEV,INPFCT,PHYDEV)
      INTEGER DSPDEV,PHYDEV
      INCLUDE 'D0$GRAPHICS_UTIL$SGIDI3:DI3GL.INC'
      IF(INPFCT.LT.1.OR.INPFCT.GT.6) RETURN
      IASSOC(INPFCT)=1
      END