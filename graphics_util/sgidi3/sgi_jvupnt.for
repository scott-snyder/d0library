C DEC/CMS REPLACEMENT HISTORY, Element JVUPNT.FOR
C *1     3-JUN-1992 14:57:51 LUPE "Release of DI3GL after split"
C DEC/CMS REPLACEMENT HISTORY, Element JVUPNT.FOR
      SUBROUTINE JVUPNT(X,Y,Z)
      INCLUDE 'D0$GRAPHICS_UTIL$SGIDI3:DI3GL.INC'
      if(idebwr.gt.0) then
        write(idebwr,8008) x,y,z
 8008   format(' JVUPNT - x,y,z:',3f)
      endif
      XVW=X
      YVW=Y
      ZVW=Z
C  RECALC VIEW VECTORS
      IF(DI3DIN.GT.0) CALL J_PROJS
      END
