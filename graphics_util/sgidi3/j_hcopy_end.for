      SUBROUTINE J_HCOPY_END
C  When the image is complete, call this routine to close the file and
C  reset everything.
      INCLUDE 'D0$GRAPHICS_UTIL$SGIDI3:DI3GL.INC'
      CALL DEV_FORCE
      CALL DEV_CLOSE_WINDOW
      HCPY=.FALSE.
      RETURN
      END
