      SUBROUTINE PS_SETLIN(LHERE)
C  Change the line style
      INCLUDE 'D0$GRAPHICS_UTIL$XWNDI3:XWNEMU.INC'
      SAVE
      LSTY=LHERE
      IF(LSTY.EQ.LOLD) RETURN
      LOLD=LSTY
      LSTY=MOD(LSTY,6)
      IF(LSTY.EQ.0) THEN
        WRITE(IDRUNI,10)
   10   FORMAT(' [] 0 sd')
      ELSEIF(LSTY.EQ.1) THEN
        WRITE(IDRUNI,20)
   20   FORMAT(' [ 20 20] 0 sd')
      ELSEIF(LSTY.EQ.2) THEN
        WRITE(IDRUNI,30)
   30   FORMAT(' [ 4  8] 0 sd')
      ELSEIF(LSTY.EQ.3) THEN
        WRITE(IDRUNI,40)
   40   FORMAT(' [ 20 16 4 16] 0 sd')
      ELSEIF(LSTY.EQ.4) THEN
        WRITE(IDRUNI,50)
   50   FORMAT(' [ 12 20] 0 sd')
      ELSEIF(LSTY.EQ.5) THEN
        WRITE(IDRUNI,60)
   60   FORMAT(' [ 40 25] 0 sd')
      ELSEIF(LSTY.EQ.6) THEN
        WRITE(IDRUNI,70)
   70   FORMAT(' [ 4 12 4 12 4 12 40 12] 0 sd')
      ELSEIF(LSTY.EQ.7) THEN
        WRITE(IDRUNI,80)
   80   FORMAT(' [ 12 16 40 16] 0 sd')
      ENDIF
      END
