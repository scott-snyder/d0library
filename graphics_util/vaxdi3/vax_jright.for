      SUBROUTINE JRIGHT(IRIGH)
      INCLUDE 'D0$INC:DI3INC.INC'
      LOGICAL IRIGH
      IF(IRIGH) THEN
        IRIGHT=1
        ARL=-1.
      ELSE
        IRIGHT=0
        ARL=1.
      ENDIF
      END
