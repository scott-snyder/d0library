C DEC/CMS REPLACEMENT HISTORY, Element JASPEK.FOR
C *1     3-JUN-1992 13:12:05 LUPE "Release of DI3GL after split"
C DEC/CMS REPLACEMENT HISTORY, Element JASPEK.FOR
      SUBROUTINE JASPEK(DSPDEV,RATIO)
      INCLUDE 'D0$GRAPHICS_UTIL$SGIDI3:DI3GL.INC'
      INTEGER DSPDEV
      RATIO=RASP
      if(idebwr.gt.0) then
        write(idebwr,8008) dspdev,ratio
 8008   format(' JASPEK - dspdev,ratio:',i,f)
      endif
      RETURN
      END
