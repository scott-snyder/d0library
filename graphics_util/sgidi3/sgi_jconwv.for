      SUBROUTINE JCONWV(X,Y,Z,VX,VY)
C-------------------------------------------------------------------
C-  Purpose and Method: Convert world coordenates to virtual 
C-  coordenates
C-
C-  Inputs: X [R]: X world coordenate
C-          Y [R]: Y world coordenate
C-          Z [R]: Z world coordenate
C-
C-  Output: VX[R]: X virtual coordenate
C-          VY[R]: Y virtual coordenate
C-
C-   Author    Mike Shupe
C-   Updated   3-DEC-1992   Lupe Howell   Clean up
C-
C-------------------------------------------------------------------
      IMPLICIT NONE
C
      REAL X,Y,Z,VX,VY
C
      INCLUDE 'D0$GRAPHICS_UTIL$SGIDI3:DI3GL.INC'
      REAL T(4,4),YTEMP,XTEMP
      EQUIVALENCE (T,TTOTAL)
C-----------------------------------------------------------------
      VX=T(1,1)*X+T(1,2)*Y+T(1,3)*Z+T(1,4)
      VY=T(2,1)*X+T(2,2)*Y+T(2,3)*Z+T(2,4)
c      if(idebwr.gt.0) then
c        write(idebwr,8008) x,y,z,vx,vy,xtemp,ytemp
c 8008   format(' JCONWV - x,y,z,vx,vy,xtemp,ytemp',6f)
c      endif
      END
