      SUBROUTINE PS_SETLIN
C  Change the line style
      INCLUDE 'D0$GRAPHICS_UTIL$SGIDI3:DI3GL.INC'
      SAVE
C!!!CERTIFY THIS BEFORE USING IT!!!
      IF(ICERT.EQ.0) RETURN
C!!!
      IF(IDLSTY.EQ.IDLOLD) RETURN
      CALL PS_FORCE
      IDTEM=IDLSTY
      IDTEM=MOD(IDTEM,6)
      IF(IDTEM.EQ.0) THEN
        WRITE(IDRUNI,10)
   10   FORMAT(' [] 0 sd')
      ELSEIF(IDTEM.EQ.1) THEN
        WRITE(IDRUNI,20)
   20   FORMAT(' [3] 0 sd')
      ELSEIF(IDTEM.EQ.2) THEN
        WRITE(IDRUNI,30)
   30   FORMAT(' [5] 0 sd')
      ELSEIF(IDTEM.EQ.3) THEN
        WRITE(IDRUNI,40)
   40   FORMAT(' [7] 0 sd')
      ELSEIF(IDTEM.EQ.4) THEN
        WRITE(IDRUNI,50)
   50   FORMAT(' [9] 0 sd')
      ELSEIF(IDTEM.EQ.5) THEN
        WRITE(IDRUNI,60)
   60   FORMAT(' [11] 0 sd')
      ENDIF
      IDLOLD=IDLSTY
      END
