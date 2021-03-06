      SUBROUTINE J_RZROTA(V1,V2,ANG)
      INCLUDE 'D0$INC:DI3INC.INC'
C  UNIT VECTORS V1,V2
C  ROTATE V1 ABOUT AXIS V2 BY ANGLE ANG
      REAL V1(3),V2(3),VN(3),V3(3),V4(3)
      ANGR=ANG*3.1415927/180.
      CSA=COS(ANGR)
      SNA=SIN(ANGR)
C  NORMALIZE V1 (ASSUME V2 IS ALREADY)
      RN=V1(1)**2+V1(2)**2+V1(3)**2
      IF(RN.EQ.0.) RETURN
      RN=SQRT(RN)
      VN(1)=V1(1)/RN
      VN(2)=V1(2)/RN
      VN(3)=V1(3)/RN
C  DOT AND CROSS
      CS=VN(1)*V2(1)+VN(2)*V2(2)+VN(3)*V2(3)
      SN=1.-CS**2
      IF(SN.LT.0.) THEN
          SN=0.
        ELSE
          SN=SQRT(SN)
      ENDIF
      IF(IRIGHT.EQ.0) THEN
          CALL J_CROSS(VN,V2,V3)
        ELSE
          CALL J_CROSS(V2,VN,V3)
      ENDIF
C  CROSS AGAIN TO PICK UP THE THIRD AXIS
      IF(IRIGHT.EQ.0) THEN
          CALL J_CROSS(V2,V3,V4)
        ELSE
          CALL J_CROSS(V3,V2,V4)
      ENDIF
C  FORM THE OUTPUT VECTOR
      DO 10 I=1,3
        V1(I)=RN*(V2(I)*CS+V3(I)*SN*SNA+V4(I)*SN*CSA)
   10 CONTINUE
      END
