      SUBROUTINE POINTR
      COMMON/BTNCOM/VXX,VYY,IBTN,IDVNOW,ICLIC
      INCLUDE 'D0$GRAPHICS_UTIL$XWNDI3:XWNEMU.INC'
C      IF(ICLIC.EQ.1) GO TO 10
C      IBTN=1
C  GET POINTER POSITION IN SCREEN COORDINATES THEN TRANSFORM
C  TO WORLD COORDINATES
c      GO=UIS$GET_POINTER_POSITION(VD_ID(IDVNOW),
c     &   WD_ID(IDVNOW),VXX,VYY)
C      ICLIC=1
      RETURN
C   10 ICLIC=0
      END