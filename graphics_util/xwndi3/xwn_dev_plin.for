      SUBROUTINE DEV_PLIN(N,P)
      REAL P(3,*)
      INCLUDE 'D0$GRAPHICS_UTIL$XWNDI3:XWNEMU.INC'
      IFILL=0
      DO I=1,N
        CALL DEV_DRAW(P(1,I),P(2,I),P(3,I))
      ENDDO
      END
