      SUBROUTINE JRDRAW(X,Y)
      INCLUDE 'D0$GRAPHICS_UTIL$SGIDI3:DI3GL.INC'
      VPOSN(1)=VPOSN(1)+X
      VPOSN(2)=VPOSN(2)+Y
      VPOSN(3)=0.
      IF(HCPY) THEN
        CALL DEV_DRAW(VPOSN(1),VPOSN(2),0.)
        RETURN
      ENDIF
      CALL DRAW2(VPOSN(1),VPOSN(2))
      END
