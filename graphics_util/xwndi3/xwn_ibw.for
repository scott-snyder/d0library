      LOGICAL FUNCTION IBW()
      INCLUDE 'D0$GRAPHICS_UTIL$XWNDI3:XWNEMU.INC'
      INCLUDE 'SYS$LIBRARY:DECW$XLIBDEF.FOR'
      RECORD /X$VISUAL/ VISU
      IBW=.FALSE.
      IF(IFIRST.EQ.1) GO TO 20
      IFIRST=1
      IBW=1
      IF(VISU.X$L_VISU_CLASS .EQ. X$C_PSEUDO_COLOR .OR.
     &   VISU.X$L_VISU_CLASS .EQ. X$C_DIRECT_COLOR) THEN
        IBW=0
      ENDIF
  20  IF(IBW.EQ.1)IBW=.TRUE.
      END
