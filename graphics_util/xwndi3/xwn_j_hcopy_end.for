      SUBROUTINE J_HCOPY_END
C  When the image is complete, call this routine to close the file and
C  reset everything.
      INCLUDE 'D0$GRAPHICS_UTIL$XWNDI3:XWNEMU.INC'
      CALL DEV_FORCE
      CALL DEV_CLOSE_WINDOW
      HCPY=.FALSE.
      RETURN
      END
