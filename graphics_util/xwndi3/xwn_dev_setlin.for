      SUBROUTINE DEV_SETLIN(N)
      INCLUDE 'D0$GRAPHICS_UTIL$XWNDI3:XWNEMU.INC'
      IDLSTY=N
      IF(IDV.EQ.1) THEN
        CALL PS_SETLIN(N)
      ELSE
C        CALL TK_SETLIN(N)
      ENDIF
      END
