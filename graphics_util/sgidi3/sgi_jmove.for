      SUBROUTINE JMOVE(X,Y)
      INCLUDE 'D0$GRAPHICS_UTIL$SGIDI3:DI3GL.INC'
      VPOSN(1)=X
      VPOSN(2)=Y
      VPOSN(3)=0.
      IF(HCPY) THEN
C        if(idebug.gt.100) type *,' Calling DEV_MOVE in JMOVE'
        CALL DEV_MOVE(X,Y,0.)
        RETURN
      ENDIF
C      if(idebug.gt.100) type *,' JMOVE-X,Y',x,y
      CALL MOVE2(X,Y)
      END
