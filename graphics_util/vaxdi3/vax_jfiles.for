      SUBROUTINE JFILES(ICODE,IDEV,LUN)
      COMMON/GUNIT/GRFUNT
      INTEGER GRFUNT
      INCLUDE 'D0$INC:DI3INC.INC'
      IF(ICODE.EQ.1)THEN      !ERROR LOGGING?
        JUNIT = LUN          !set logical unit for prints
      ELSEIF(ICODE.EQ.3)THEN  !GRAPHICS OUTPUT FILE?
        GRFUNT = LUN          !specify output file LUN
      ENDIF
      END
