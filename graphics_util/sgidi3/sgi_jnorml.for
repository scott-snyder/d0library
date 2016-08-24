C DEC/CMS REPLACEMENT HISTORY, Element JNORML.FOR
C *1     3-JUN-1992 14:07:58 LUPE "Release of DI3GL after split"
C DEC/CMS REPLACEMENT HISTORY, Element JNORML.FOR
      SUBROUTINE JNORML(X,Y,Z)
      INCLUDE 'D0$GRAPHICS_UTIL$SGIDI3:DI3GL.INC'
      R2=X*X+Y*Y+Z*Z
      IF(IDEBUG.GT.0) TYPE *,' IN JNORML: X,Y,Z:',X,Y,Z
      if(idebwr.gt.0) then
        write(idebwr,8008) x,y,z
 8008   format(' JNORML - x,y,x:',3f)
      endif
      IF(R2.LE.0)THEN
        WRITE(JUNIT,*)'JNORML CALLED WITH ZERO VECTOR'
        RETURN
      ENDIF
      R2=1./SQRT(R2)
      VNORMX=X*R2
      VNORMY=Y*R2
      VNORMZ=Z*R2
C  RECALC VIEWING VECTORS
      IF(DI3DIN.GT.0) CALL J_PROJS
      END