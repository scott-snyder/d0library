      SUBROUTINE JALOAD(IARRAY)
      INCLUDE 'D0$GRAPHICS_UTIL$XWNDI3:XWNEMU.INC'
      INTEGER IARRAY(23)
      DO I=1,10
        IJ1(I+4)=IARRAY(I)
      ENDDO
      DO I=1,10
        IJ1(I+15)=IARRAY(I+10)
      ENDDO
      DO I=1,3
        IJ1(I+31)=IARRAY(I+20)
      ENDDO
      END