      SUBROUTINE J_J1STRG(STRING)
C  OUTPUT A HARDWARE FONT STRING
      INCLUDE 'D0$GRAPHICS_UTIL$XWNDI3:XWNEMU.INC'
      CHARACTER*(*) STRING
      CALL J_TR3XYZ(XPOSN,YPOSN,ZPOSN,XSCR,YSCR,CDUMMY)
      CALL J_TRDEV(XSCR,YSCR,XP,YP)
C  (Justify inside the J_TEXT routine.)
      CALL J_TEXT(STRING,XP,YP)
      END