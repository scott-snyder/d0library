      SUBROUTINE JIENAB(DSPDEV,INPFCT,PHYDEV)
      INTEGER DSPDEV,PHYDEV
      INCLUDE 'D0$GRAPHICS_UTIL$XWNDI3:XWNEMU.INC'
      IF(INPFCT.LT.1.OR.INPFCT.GT.6) RETURN
      IASSOC(INPFCT)=1
      END
