      SUBROUTINE D_MATROT(ANGLE,AXIS,TMATR)
C  Rotate by angle ANGLE in degrees, about AXIS. (Right hand rule).
      CHARACTER*1 AXIS
      INCLUDE 'D0$GRAPHICS_UTIL$SGIDI3:DI3GL.INC'
      REAL TROT(4,4),TMATR(4,4)
      DATA PI/3.1415927/
C
      A=ANGLE*PI/180.
      SN=SIN(A)
      CS=COS(A)
      IF(AXIS.EQ.'X'.OR.AXIS.EQ.'x') THEN
        I1=3
        I2=2
      ELSEIF(AXIS.EQ.'Y'.OR.AXIS.EQ.'y') THEN
        I1=1
        I2=3
      ELSEIF(AXIS.EQ.'Z'.OR.AXIS.EQ.'z') THEN
        I1=2
        I2=1
      ENDIF
      CALL D_MATUNI(TROT)
      TROT(I1,I1)=CS
      TROT(I1,I2)=SN
      TROT(I2,I1)=-SN
      TROT(I2,I2)=CS
      CALL D_MATMUL(TROT,TMATR)
      END
