      SUBROUTINE DEV_ROT(ANGLE,AXIS)
C  Rotate by angle ANGLE in degrees, about AXIS. (Right hand rule).
      CHARACTER*1 AXIS
      INCLUDE 'D0$GRAPHICS_UTIL$XWNDI3:XWNEMU.INC'
      DATA PI/3.1415927/
      REAL TROT(4,4)
      A=ANGLE*PI/180.
      SN=SIN(A)
      CS=COS(A)
      IF(AXIS.EQ.'X'.OR.AXIS.EQ.'x') THEN
        I1=2
        I2=3
      ELSEIF(AXIS.EQ.'Y'.OR.AXIS.EQ.'y') THEN
        I1=1
        I2=3
      ELSEIF(AXIS.EQ.'Z'.OR.AXIS.EQ.'z') THEN
        I1=1
        I2=2
      ENDIF
      CALL D_MATUNI(TROT)
      TROT(I1,I1)=CS
      TROT(I1,I2)=-SN
      TROT(I2,I1)=SN
      TROT(I2,I2)=CS
      CALL D_MATMUL(TMODEL,TROT)
C!!!THESE MAY NOT BE IN THE RIGHT ORDER
      CALL D_MATCPY(TTOTAL,TDEVIC)
      CALL D_MATMUL(TTOTAL,TMODEL)
c      type *,' DEV_ROT-TTOTAL - ',ttotal
      END
