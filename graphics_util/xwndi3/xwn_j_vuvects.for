      SUBROUTINE J_VUVECTS
C  FIND THE U AND V UNIT VECTORS FROM THE CURRENT N VECTOR (NORM.)
C  AND UP VECTOR (NOT NECESSARILY NORM)
      INCLUDE 'D0$GRAPHICS_UTIL$XWNDI3:XWNEMU.INC'
      IF(IRIGHT.EQ.0) THEN
        CALL J_CROSS(UPVEC,NVEC,UVEC)
      ELSE
        CALL J_CROSS(NVEC,UPVEC,UVEC)
      ENDIF
      RU=UVECX**2+UVECY**2+UVECZ**2
      IF(RU.LE.0.) GO TO 999
      RU=SQRT(RU)
      UVECX=UVECX/RU
      UVECY=UVECY/RU
      UVECZ=UVECZ/RU
      IF(IRIGHT.EQ.0) THEN
        CALL J_CROSS(NVEC,UVEC,VVEC)
      ELSE
        CALL J_CROSS(UVEC,NVEC,VVEC)
      ENDIF
C  CALL J_COORDS SO THAT NEW VIEW VECTORS APPEAR IN THE HARDCOPY
C  TRANSFORMATION MATRIX.
      CALL J_COORDS
  999 RETURN
      END
