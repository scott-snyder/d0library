      SUBROUTINE DELETE_DISPLAY
      INCLUDE 'D0$GRAPHICS_UTIL$XWNDI3:XWNEMU.INC'
      CALL X$UNMAP_WINDOW(VD_ID,WD_ID)
      CALL X$DESTROY_WINDOW(VD_ID,WD_ID)
      CALL X$CLOSE_DISPLAY(VD_ID)
      END
