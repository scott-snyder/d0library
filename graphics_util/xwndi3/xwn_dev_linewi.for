      SUBROUTINE DEV_LINEWI(N)
      INCLUDE 'D0$GRAPHICS_UTIL$XWNDI3:XWNEMU.INC'
      IDLWID=N
      IF(IDV.EQ.1) THEN
        CALL PS_LINEWI
      ELSE
C        CALL TK_LINEWI
      ENDIF
      END