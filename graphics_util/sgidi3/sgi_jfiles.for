C DEC/CMS REPLACEMENT HISTORY, Element JFILES.FOR
C *1     3-JUN-1992 13:52:58 LUPE "Release of DI3GL after split"
C DEC/CMS REPLACEMENT HISTORY, Element JFILES.FOR
      SUBROUTINE JFILES(ICODE,IDEV,LUN)
      COMMON/J_GUNIT/GRFUNT
      INTEGER GRFUNT
      INCLUDE 'D0$GRAPHICS_UTIL$SGIDI3:DI3GL.INC'
      IF(ICODE.EQ.1)THEN           !ERROR LOGGING?
        JUNIT=LUN                  !set logical unit for prints
      ELSEIF(ICODE.EQ.3)THEN       !GRAPHICS OUTPUT FILE?
        GRFUNT=LUN                 !specify output file LUN
      ENDIF
      END
