C DEC/CMS REPLACEMENT HISTORY, Element JDJUST.FOR
C *1     3-JUN-1992 13:44:13 LUPE "Release of DI3GL after split"
C DEC/CMS REPLACEMENT HISTORY, Element JDJUST.FOR
      SUBROUTINE JDJUST(IHORIZ,IVERT)
      INCLUDE 'D0$GRAPHICS_UTIL$SGIDI3:DI3GL.INC'
      IF(IHORIZ.LT.1.OR.IHORIZ.GT.3)THEN
        WRITE(JUNIT,*)'JDJUST HORIZONTAL NO GOOD:',IHORIZ
        RETURN
      ENDIF
      IF(IVERT.LT.1.OR.IVERT.GT.3)THEN
        WRITE(JUNIT,*)'JDJUST VERTICAL NO GOOD:',IVERT
        RETURN
      ENDIF
      IHJUDF=IHORIZ
      IVJUDF=IVERT
      CALL JJUST(IHORIZ,IVERT)
      END
