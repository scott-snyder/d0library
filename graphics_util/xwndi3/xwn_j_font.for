      SUBROUTINE J_FONT(ISFONT)
C-  Select font number for stroke fonts
      INCLUDE 'D0$GRAPHICS_UTIL$XWNDI3:XWNEMU.INC'
      INTEGER IFMAP(20)
      DATA IFMAP/0,1,0,1,2,3,2,3,4,5,6,7,0,1,10,11,0,1,0,1/
C-----------------------------
      IF(ISFONT.LT.1.OR.ISFONT.GT.20) THEN
        IFONTY=0
      ELSE
        IFONTY=IFMAP(ISFONT)
      ENDIF
      IOFFSET=0                        ! CDF or Roman
      IF(IFONTY.GE.4.AND.IFONTY.LE.5) THEN
        IOFFSET=128                    ! Greek
      ELSEIF(IFONTY.GE.6.AND.IFONTY.LE.7) THEN
        IOFFSET=256                    ! Script
      ELSEIF(IFONTY.GE.8.AND.IFONTY.LE.9) THEN
        IOFFSET=384                    ! Tiny
      ELSEIF(IFONTY.GE.10.AND.IFONTY.LE.11) THEN
        IOFFSET=224                    ! OLD ENGLISH (CAPS ONLY)
      ENDIF
      RETURN
      END
