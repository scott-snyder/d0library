      SUBROUTINE DEV_FORCE
C  Force output from buffered devices.
      INCLUDE 'D0$GRAPHICS_UTIL$SGIDI3:DI3GL.INC'
      IF(IDV.EQ.1) THEN
        CALL PS_FORCE
      ELSEIF(IDV.EQ.2) THEN
        CALL TK_FORCE
      ELSEIF(IDV.EQ.3) THEN
        CALL TK_FORCE
      ELSEIF(IDV.EQ.4) THEN
        CALL QM_FORCE
      ENDIF
      END
