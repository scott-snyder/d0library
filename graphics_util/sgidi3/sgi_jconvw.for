      SUBROUTINE JCONVW(VX,VY,X,Y,Z)
C  CONVERT VIRTUAL WINDOW TO WORLD COORDINATES
      INCLUDE 'D0$GRAPHICS_UTIL$SGIDI3:DI3GL.INC'
      REAL T(4,4)
      EQUIVALENCE (T,TTOTAL)
C
      DEN=T(1,2)*T(2,1)-T(2,2)*T(1,1)
      IF(DEN.NE.0. .AND. T(1,1).NE.0.) THEN
        Y=(T(2,1)*(VX-T(1,4))-T(1,1)*(VY-T(2,4)))/DEN
        X=(VX-T(1,4)-Y*T(1,2))/T(1,1)
      ELSE
        Y=0.
        X=0.
      ENDIF
C      type *,'JCONVW - VX,VY,X,Y'
C      type *,VX,VY,X,Y
      if(idebwr.gt.0) then
        write(idebwr,8003) vx,vy,x,y
 8003   format(' JCONVW - vx,vy,x,y',4f)
      endif
      END
