      SUBROUTINE DEV_TRANSF(X,Y,Z,XOUT,YOUT)
C  Apply the full transformation matrix to convert from world to
C  screen or page coordinates.
      INCLUDE 'D0$GRAPHICS_UTIL$SGIDI3:DI3GL.INC'
      REAL T(4,4)
      EQUIVALENCE (T,TTOTAH)
      XOUT=T(1,1)*X+T(1,2)*Y+T(1,3)*Z+T(1,4)
      YOUT=T(2,1)*X+T(2,2)*Y+T(2,3)*Z+T(2,4)
C      TYPE *,' XOUT,XTEMP,YOUT,YTEMP,XSCAL:'
C      TYPE *,XOUT,YOUT,XTEMP,YTEMP,XSCAL
      END
