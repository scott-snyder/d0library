      SUBROUTINE J_RZTRNN(VIN,VOUT,BUMP)
C  TRANSFORM THE IN VECTOR INTO THE OUT VECTOR WITH A TRANSLATION
C  BUMP IN THE DIRECTION OF NVEC
      INCLUDE 'D0$GRAPHICS_UTIL$XWNDI3:XWNEMU.INC'
      REAL VIN(3),VOUT(3)
      DO I=1,3
        VOUT(I)=VIN(I)+BUMP*NVEC(I)
      ENDDO
      END
