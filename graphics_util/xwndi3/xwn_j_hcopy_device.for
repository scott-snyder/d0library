      SUBROUTINE J_HCOPY_DEVICE(IDEVI,FILENAME)
C  Initialization routine to choose a hardcopy device.
C  IDEVI    = 1 POSTSCRIPT HARDCOPY
C           = 2 TEKTRONIX HARDCOPY
C           = 3 TEKTRONIX TERMINAL
C  FILENAME = HARDCOPY FILENAME.  IF BLANK, WILL GIVE DEFAULT
C----------------------------------------------------------------------
      INCLUDE 'D0$GRAPHICS_UTIL$XWNDI3:XWNEMU.INC'
      CHARACTER*(*) FILENAME
      IHCDEV=IDEVI
      IF(FILENAME(1:1).NE.' ') HFILNAME=FILENAME
      RETURN
      END
