C DEC/CMS REPLACEMENT HISTORY, Element JUPVEC.FOR
C *1     3-JUN-1992 14:52:32 LUPE "Release of DI3GL after split"
C DEC/CMS REPLACEMENT HISTORY, Element JUPVEC.FOR
      SUBROUTINE JUPVEC(X,Y,Z)
      INCLUDE 'D0$GRAPHICS_UTIL$SGIDI3:DI3GL.INC'
      R2=X*X+Y*Y+Z*Z
      if(idebwr.gt.0) then
        write(idebwr,8008) x,y,z
 8008   format(' JUPVEC - x,y,z:',3f)
      endif
      IF(IDEBUG.GT.0) TYPE *,' IN JUPVEC: X,Y,Z:',X,Y,Z
      IF(R2.LE.0)THEN
        WRITE(JUNIT,*)'JUPVEC CALLED WITH ZERO VECTOR'
        RETURN
      ENDIF
      R2=1./SQRT(R2)
      VUPX=X*R2
      VUPY=Y*R2
      VUPZ=Z*R2
C  RECALC VIEW VECTORS
      IF(DI3DIN.GT.0) CALL J_PROJS
      END
