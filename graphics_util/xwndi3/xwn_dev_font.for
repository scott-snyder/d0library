      SUBROUTINE DEV_FONT(INDEX)
      INCLUDE 'D0$GRAPHICS_UTIL$XWNDI3:XWNEMU.INC'
      IF(IDV.EQ.1) THEN
        CALL PS_FONT(INDEX)
      ELSE
C        CALL TK_FONT(INDEX)
      ENDIF
      END
