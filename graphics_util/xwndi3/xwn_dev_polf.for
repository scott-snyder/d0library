      SUBROUTINE DEV_POLF(N,PARRY)
      REAL PARRY(3,*)
      INCLUDE 'D0$GRAPHICS_UTIL$XWNDI3:XWNEMU.INC'
      IFILL=1
      IF(IDV.EQ.1) THEN
        CALL PS_POLY(N,PARRY)
      ELSE
        CALL TK_POLY(N,PARRY)
      ENDIF
      IFILL=0
      END
