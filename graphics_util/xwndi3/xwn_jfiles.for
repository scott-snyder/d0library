      SUBROUTINE JFILES(ICODE,IDEV,LUN)
      INCLUDE 'D0$GRAPHICS_UTIL$XWNDI3:XWNEMU.INC'
      IF(ICODE.EQ.1)THEN      !ERROR LOGGING?
        JUNIT = LUN           !set logical unit for prints
      ELSEIF(ICODE.EQ.3)THEN  !GRAPHICS OUTPUT FILE?
        GRFUNT = LUN          !specify output file LUN
      ENDIF
      END
