      SUBROUTINE JTRANS(MAT,IT,P1,P2,P3,P4,P5,P6,P7)
      REAL*4 MAT(4,4)
C
      COMMON/DI3PR/JUNIT
      IF(IT.LT.1.OR.IT.GT.11)GO TO 1000
C CLEAR FIRST
      DO 111 I=1,4
        DO 112 J=1,4
          MAT(I,J)=0.
  112   CONTINUE
  111 CONTINUE
      GO TO (10,10,30,40,40,40,40,40,40,100,110) IT
   10 DO 120 I=1,4
        MAT(I,I)=1.
  120 CONTINUE
      IF(IT.EQ.1)RETURN
      MAT(4,1)=P1
      MAT(4,2)=P2
      MAT(4,3)=P3
      RETURN
   30 MAT(1,1)=P4
      MAT(2,2)=P5
      MAT(3,3)=P6
      MAT(4,1)=P4*(1.-P1)
      MAT(4,2)=P5*(1.-P2)
      MAT(4,3)=P6*(1.-P3)
      RETURN
   40 ANG=P1
      IF(IT.GT.6)ANG=ANG*3.1415927/180.
      SN=SIN(ANG)
      CS=COS(ANG)
      GO TO (41,42,43,41,42,43) IT-3
   41 MAT(2,2)=CS
      MAT(3,3)=CS
C FIX!!
C      MAT(3,2)=-ARL*SN
C      MAT(2,3)= ARL*SN
      MAT(3,2)=SN
      MAT(2,3)=-SN
      MAT(1,1)=1.
      MAT(4,4)=1.
      RETURN
   42 MAT(1,1)=CS
      MAT(3,3)=CS
      MAT(3,1)=SN
      MAT(1,3)=-SN
      MAT(2,2)=1.
      MAT(4,4)=1.
      RETURN
   43 MAT(1,1)=CS
      MAT(2,2)=CS
      MAT(2,1)=SN
      MAT(1,2)=-SN
      MAT(3,3)=1.
      MAT(4,4)=1.
      RETURN
  100 RETURN
  110 RETURN
 1000 WRITE(JUNIT,*)'JTRANS: ILLEGAL TRCODE'
      END
