      SUBROUTINE J_LINE2(X1,Y1,X2,Y2)
      INCLUDE 'D0$GRAPHICS_UTIL$XWNDI3:XWNEMU.INC'
      INCLUDE 'SYS$LIBRARY:DECW$XLIBDEF.FOR'
      CALL J_TRDEV(X1,Y1,XP1,YP1)
      CALL J_TRDEV(X2,Y2,XP2,YP2)
C  XWINDOWS ORIGIN IS AT UPPER LEFT.  (REVENGE OF THE BRASS RAT.)
      IX1=XP1
      IY1=HEIGHT-YP1
      IX2=XP2
      IY2=HEIGHT-YP2
      IF(IDEBUG.GT.30) THEN
        TYPE *,' X$DRAW_LINE-X1,Y1,X2,Y2,IX1,IY1,IX2,IY2:',
     &                       X1,Y1,X2,Y2,IX1,IY1,IX2,IY2
      ENDIF
      CALL X$DRAW_LINE(VD_ID,WD_ID,ATB(IATB),IX1,IY1,IX2,IY2)
      END
