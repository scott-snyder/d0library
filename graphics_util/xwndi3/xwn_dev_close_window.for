      SUBROUTINE DEV_CLOSE_WINDOW
C  Close display.  Reenable workstation as default.
      INCLUDE 'D0$GRAPHICS_UTIL$XWNDI3:XWNEMU.INC'
      IF(IDV.EQ.1) THEN
        CALL PS_FORCE
        CALL PS_FIN
        CLOSE(IDRUNI)
c        TYPE *,' HARDCOPY DONE'
      ELSEIF(IDV.EQ.2) THEN
        CALL TK_FORCE
        CALL TK_FIN
        CLOSE(IDRUNI)
c        TYPE *,' HARDCOPY DONE'
      ELSEIF(IDV.EQ.4) THEN
        CALL QM_FORCE
        CALL QM_FIN
        CLOSE(IDRUNI)
c        TYPE *,' HARDCOPY DONE'
      ELSEIF(IDV.EQ.3) THEN
        CALL CLOSE_TKWIND
      ENDIF
      IGSYNC=2
      RETURN
      END
