      SUBROUTINE DEV_PAGE
      INCLUDE 'D0$GRAPHICS_UTIL$XWNDI3:XWNEMU.INC'
      IF(IDV.EQ.1) THEN
        CALL PS_PAGE
      ELSEIF(IDV.EQ.2) THEN
C!!!        CALL TK_PAGE
      ELSEIF(IDV.EQ.4) THEN
C!!!        CALL QM_PAGE
      ENDIF
      END
