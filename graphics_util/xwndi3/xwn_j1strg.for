      SUBROUTINE J1STRG(STRING)
C  OUTPUT A HARDWARE FONT STRING
      INCLUDE 'D0$GRAPHICS_UTIL$XWNDI3:XWNEMU.INC'
      CHARACTER*(*) STRING
C      IF(STFONT) THEN
C        CALL J3STRG(STRING)
C        RETURN
C      ENDIF
      IF(HCPY) THEN
        CALL DEV_TEXT(STRING)
      ELSE
        CALL J_J1STRG(STRING)
      ENDIF
      IF(.NOT.PUTS)RETURN
      ILEN=LEN(STRING)
      CALL J_PUTSTR(I1STRG,ILEN,STRING)
      END