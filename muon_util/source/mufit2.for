C DEC/CMS REPLACEMENT HISTORY, Element MUFIT2.FOR
C *5     7-FEB-1990 23:17:50 HEDIN "ONE HIT/PLANE"
C *4     1-MAY-1989 15:01:06 TAMI "Setup for MC"
C *3    10-APR-1989 13:31:43 TAMI ""
C *2     4-MAR-1989 15:36:26 HEDIN "weight magnet point"
C *1    20-JAN-1989 12:14:33 TAMI "Loop over L/R drift combos"
C DEC/CMS REPLACEMENT HISTORY, Element MUFIT2.FOR
      SUBROUTINE MUFIT2(N,JJ,Z,D,WT,IS,X1,Z1,SL1,C1)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC     LOOP OVER L/R DRIFT COMBINATIONS; USE OPTIMIZED CALCULATION
CC     ASSUMES ALL POINTS ARE GOOD. HAS MAXIMUM OF 10 POINTS ON FIT
CC     MIMIMUM OF 3. IGNORE WEIGHTS FOR NOW THOUGH COMMENT OUT CODE
CC
CC     INPUT: N          = NUMBER OF POINTS (GE.3)
CC            JJ         = NUMBER OF TIMES (1 OR 2)
CC            Z          = POSITION BETWEEN PLANES
CC            D          = DRIFT DISTANCE+WIRE
CC            WT         = WEIGHT OF EACH POINT
CC     OUTPUT: IS        = DRIFT SOLUTION 
CC             X1        = FITTED POSITION (DRIFT DIRECTION)
CC             Z1        = CENTER OF GRAVITY BETWEEN PLANES FROM FIT
CC            SL1        = SLOPE DX/DZ
CC             C1        = SQRT(CHI**2/(N-2))         
CC
CC     HEDIN 12/6/88; 1/89 PLAY WITH WEIGHTING FACTOR FIRST POINT ONLY
CC     DH 1/90 GO TO ONE HIT/PLANE
CC     DH 4/91 CHANGE MAX TO 12
CC     DH 5/91 minor change; cleanup nested do loop
CC     DH 7/91 add quick early exits for bad fits
CC     DH 1/93 allow weight for first 4 points
CC     DH 2/93 double precision for CHI calculation
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE
      INTEGER N,NFIT,I
      PARAMETER (NFIT=12)
      REAL X1,Z1,SL1,C1,ZQ,FACTOR
      REAL WT(NFIT),Z(NFIT),X(NFIT),D(12,NFIT)
      REAL*8 SZ2B(NFIT),ZQB(NFIT),SWB(NFIT),SZB(NFIT)
      REAL X0A,SLA,ZA,CHIMX,CHI,VXG,VSL
      REAL*8 SW,SX,SZ,SX2,SZ2,SXZ,SX4,SX5,SX6,SX7,SX8,SX9,SX10,
     A SX24,SX25,SX26,SX27,SX28,SX29,SX210,SXZ4,SXZ5,SXZ6,SXZ7,
     A SXZ8,SXZ9,SXZ10,SXZ11,SXZ12,SX11,SX12,SX211,SX212
      INTEGER JJ(NFIT),IS(NFIT)
      INTEGER I1,I2,I3,I4,I5,I6,I7,I8,I9,I10,I11,I12
      PARAMETER (FACTOR=2.0)
C
C
      IF(N.LE.2.OR.N.GT.NFIT) THEN   ! TOO FEW OR TOO MANY POINTS
        C1=-1.
        SL1=10000.
        Z1=-100000.
        X1=-999999.
        RETURN
      ENDIF
C
      SW=0.
      SZ=0.
      SZ2=0.
      DO I=1,N
        SW=SW+1./WT(I)**2
        SZ=SZ+Z(I)/WT(I)**2
        SZ2=SZ2+Z(I)**2/WT(I)**2
C        SZ=SZ+Z(I)
C        SZ2=SZ2+Z(I)**2
        SWB(I)=SW
        SZB(I)=SZ
        SZ2B(I)=SZ2
        ZQB(I)=SZ2-SZ**2/SW
      ENDDO
C      SW=N
      ZQ=SZ2-SZ**2/SW
C    LOOP OVER ALL DRIFT SOLUTIONS
C
C     J is for number of solutions in that plane
C
      CHIMX=100000.
C
      DO 302 I1=1,JJ(1)
      X(1)= D(I1,1)
      DO 301 I2=1,JJ(2)
      X(2)= D(I2,2)
      DO 3 I3=1,JJ(3)
      X(3)= D(I3,3)
C
      SX=X(1)/WT(1)**2+X(2)/WT(2)**2+X(3)/WT(3)**2
      SX2=X(1)**2/WT(1)**2+X(2)**2/WT(2)**2+X(3)**2/WT(3)**2
      SXZ=X(1)*Z(1)/WT(1)**2+X(2)*Z(2)/WT(2)**2+X(3)*Z(3)/WT(3)**2
C      SX=X(1)+X(2)+X(3)
C      SX2=X(1)**2+X(2)**2+X(3)**2 
C      SXZ=X(1)*Z(1)+X(2)*Z(2)+X(3)*Z(3) 
C
      DO 202 I4=1,JJ(4)
      IF(N.GE.4) THEN
        X(4)= D(I4,4)
        SX4=SX+X(4)/WT(4)**2
        SX24=SX2+X(4)**2/WT(4)**2
        SXZ4=SXZ+X(4)*Z(4)/WT(4)**2
C        SX4=SX+X(4) 
C        SX24=SX2+X(4)**2 
C        SXZ4=SXZ+X(4)*Z(4) 
        CHI=(SX24-SX4**2/SWB(4)-(SXZ4-SX4*SZB(4)/SWB(4))**2/ZQB(4))/2.
        IF(CHI.GT.FACTOR*CHIMX) GO TO 202
      ENDIF
      DO 2 I5=1,JJ(5)
      IF(N.GE.5) THEN
      X(5)= D(I5,5)
C        SX5=SX4+X(5)/WT(5)**2
C        SX25=SX24+X(5)**2/WT(5)**2
C        SXZ5=SXZ4+X(5)*Z(5)/WT(5)**2
        SX5=SX4+X(5) 
        SX25=SX24+X(5)**2 
        SXZ5=SXZ4+X(5)*Z(5) 
        CHI=(SX25-SX5**2/SWB(5)-(SXZ5-SX5*SZB(5)/SWB(5))**2/ZQB(5))/3.
        IF(CHI.GT.FACTOR*CHIMX) GO TO 2
      ENDIF
      DO 102 I6=1,JJ(6)
      IF(N.GE.6) THEN
      X(6)= D(I6,6)
C        SX6=SX5+X(6)/WT(6)**2
C        SX26=SX25+X(6)**2/WT(6)**2
C        SXZ6=SXZ5+X(6)*Z(6)/WT(6)**2
        SX6=SX5+X(6) 
        SX26=SX25+X(6)**2 
        SXZ6=SXZ5+X(6)*Z(6) 
        CHI=(SX26-SX6**2/SWB(6)-(SXZ6-SX6*SZB(6)/SWB(6))**2/ZQB(6))/4.
        IF(CHI.GT.FACTOR*CHIMX) GO TO 102
      ENDIF
      DO 1 I7=1,JJ(7) 
      IF(N.GE.7) THEN
      X(7)= D(I7,7)
C        SX7=SX6+X(7)/WT(7)**2
C        SX27=SX26+X(7)**2/WT(7)**2
C        SXZ7=SXZ6+X(7)*Z(7)/WT(7)**2
        SX7=SX6+X(7) 
        SX27=SX26+X(7)**2 
        SXZ7=SXZ6+X(7)*Z(7) 
        CHI=(SX27-SX7**2/SWB(7)-(SXZ7-SX7*SZB(7)/SWB(7))**2/ZQB(7))/5.
        IF(CHI.GT.FACTOR*CHIMX) GO TO 1
      ENDIF
      DO 404 I8=1,JJ(8)
      IF(N.GE.8) THEN
      X(8)= D(I8,8)
C        SX8=SX7+X(8)/WT(8)**2
C        SX28=SX27+X(8)**2/WT(8)**2
C        SXZ8=SXZ7+X(8)*Z(8)/WT(8)**2
        SX8=SX7+X(8) 
        SX28=SX27+X(8)**2 
        SXZ8=SXZ7+X(8)*Z(8) 
        CHI=(SX28-SX8**2/SWB(8)-(SXZ8-SX8*SZB(8)/SWB(8))**2/ZQB(8))/6.
        IF(CHI.GT.FACTOR*CHIMX) GO TO 404
      ENDIF
      DO 403 I9=1,JJ(9)
      IF(N.GE.9) THEN
      X(9)= D(I9,9)
C        SX9=SX8+X(9)/WT(9)**2
C        SX29=SX28+X(9)**2/WT(9)**2
C        SXZ9=SXZ8+X(9)*Z(9)/WT(9)**2
        SX9=SX8+X(9) 
        SX29=SX28+X(9)**2 
        SXZ9=SXZ8+X(9)*Z(9) 
        CHI=(SX29-SX9**2/SWB(9)-(SXZ9-SX9*SZB(9)/SWB(9))**2/ZQB(9))/7.
        IF(CHI.GT.FACTOR*CHIMX) GO TO 403
      ENDIF
      DO 402 I10=1,JJ(10)
C      IF(N.GE.10)  SX10=SX9+X(10)/WT(10)**2
C      IF(N.GE.10)  SX210=SX29+X(10)**2/WT(10)**2
C      IF(N.GE.10)  SXZ10=SXZ9+X(10)*Z(10)/WT(10)**2
      IF(N.GE.10) THEN
      X(10)= D(I10,10)
        SX10=SX9+X(10) 
        SX210=SX29+X(10)**2 
        SXZ10=SXZ9+X(10)*Z(10)
        CHI=(SX210-SX10**2/SWB(10)-(SXZ10-SX10*SZB(10)/SWB(10))**2
     A  /ZQB(10))/8.
        IF(CHI.GT.FACTOR*CHIMX) GO TO 402
      ENDIF 
      DO 401 I11=1,JJ(11)
C      IF(N.GE.11)  SX11=SX10+X(11)/WT(11)**2
C      IF(N.GE.11)  SX211=SX210+X(11)**2/WT(11)**2
C      IF(N.GE.11)  SXZ11=SXZ10+X(11)*Z(11)/WT(11)**2
      IF(N.GE.11) THEN
      X(11)= D(I11,11)
        SX11=SX10+X(11) 
        SX211=SX210+X(11)**2 
        SXZ11=SXZ10+X(11)*Z(11)
        CHI=(SX211-SX11**2/SWB(11)-(SXZ11-SX11*SZB(11)/SWB(11))**2
     A  /ZQB(11))/9.
        IF(CHI.GT.FACTOR*CHIMX) GO TO 401
      ENDIF 
      DO 4 I12=1,JJ(12)
C      IF(N.GE.12)  SX12=SX11+X(12)/WT(12)**2
C      IF(N.GE.12)  SX212=SX211+X(12)**2/WT(12)**2
C      IF(N.GE.12)  SXZ12=SXZ11+X(12)*Z(12)/WT(12)**2
      IF(N.GE.12) THEN
      X(12)= D(I12,12)
        SX12=SX11+X(12) 
        SX212=SX211+X(12)**2 
        SXZ12=SXZ11+X(12)*Z(12)
        CHI=(SX212-SX12**2/SWB(12)-(SXZ12-SX12*SZB(12)/SWB(12))**2
     A  /ZQB(12))/10.
        IF(CHI.GT.FACTOR*CHIMX) GO TO 4
      ENDIF 
C
      IF(N.EQ.3) THEN
       CHI=SX2-SX**2/SW-(SXZ-SX*SZ/SW)**2/ZQ
C     ELSE IF(N.EQ.4) THEN
C      CHI=SX24-SX4**2/SW-(SXZ4-SX4*SZ/SW)**2/ZQ
C     ELSE IF(N.EQ.5) THEN
C      CHI=SX25-SX5**2/SW-(SXZ5-SX5*SZ/SW)**2/ZQ
C     ELSE IF(N.EQ.6) THEN
C      CHI=SX26-SX6**2/SW-(SXZ6-SX6*SZ/SW)**2/ZQ
C     ELSE IF(N.EQ.7) THEN
C      CHI=SX27-SX7**2/SW-(SXZ7-SX7*SZ/SW)**2/ZQ
C     ELSE IF(N.EQ.8) THEN
C      CHI=SX28-SX8**2/SW-(SXZ8-SX8*SZ/SW)**2/ZQ
C     ELSE IF(N.EQ.9) THEN
C      CHI=SX29-SX9**2/SW-(SXZ9-SX9*SZ/SW)**2/ZQ
C     ELSE IF(N.EQ.10) THEN
C      CHI=SX210-SX10**2/SW-(SXZ10-SX10*SZ/SW)**2/ZQ
C     ELSE IF(N.EQ.11) THEN
C      CHI=SX211-SX11**2/SW-(SXZ11-SX11*SZ/SW)**2/ZQ
C     ELSE IF(N.EQ.12) THEN
C      CHI=SX212-SX12**2/SW-(SXZ12-SX12*SZ/SW)**2/ZQ
      ENDIF
C
C       IF(CHI.LT.0.) GO TO 4
       IF(CHI.GT.CHIMX) GO TO 4
      IF(N.EQ.3) THEN
       CALL MUSLN(SW,SX,SZ,SX2,SZ2,SXZ,X0A,ZA,SLA,VXG,VSL,CHI)
      ELSE IF(N.EQ.4) THEN
       CALL MUSLN(SW,SX4,SZ,SX24,SZ2,SXZ4,X0A,ZA,SLA,VXG,VSL,CHI)
      ELSE IF(N.EQ.5) THEN
       CALL MUSLN(SW,SX5,SZ,SX25,SZ2,SXZ5,X0A,ZA,SLA,VXG,VSL,CHI)
      ELSE IF(N.EQ.6) THEN
       CALL MUSLN(SW,SX6,SZ,SX26,SZ2,SXZ6,X0A,ZA,SLA,VXG,VSL,CHI)
      ELSE IF(N.EQ.7) THEN
       CALL MUSLN(SW,SX7,SZ,SX27,SZ2,SXZ7,X0A,ZA,SLA,VXG,VSL,CHI)
      ELSE IF(N.EQ.8) THEN
       CALL MUSLN(SW,SX8,SZ,SX28,SZ2,SXZ8,X0A,ZA,SLA,VXG,VSL,CHI)
      ELSE IF(N.EQ.9) THEN
       CALL MUSLN(SW,SX9,SZ,SX29,SZ2,SXZ9,X0A,ZA,SLA,VXG,VSL,CHI)
      ELSE IF(N.EQ.10) THEN
       CALL MUSLN(SW,SX10,SZ,SX210,SZ2,SXZ10,X0A,ZA,SLA,VXG,VSL,CHI)
      ELSE IF(N.EQ.11) THEN
       CALL MUSLN(SW,SX11,SZ,SX211,SZ2,SXZ11,X0A,ZA,SLA,VXG,VSL,CHI)
      ELSE IF(N.EQ.12) THEN
       CALL MUSLN(SW,SX12,SZ,SX212,SZ2,SXZ12,X0A,ZA,SLA,VXG,VSL,CHI)
      ENDIF
        CHIMX=CHI/(N-2.)
        C1=CHI/(N-2.)
        X1=X0A
        Z1=ZA
        SL1=SLA
        IS(1)=I1
        IS(2)=I2
        IS(3)=I3
        IS(4)=I4
        IS(5)=I5
        IS(6)=I6
        IS(7)=I7
        IS(8)=I8
        IS(9)=I9
        IS(10)=I10
        IS(11)=I11
        IS(12)=I12
4     CONTINUE
401   CONTINUE
402   CONTINUE
403   CONTINUE
404   CONTINUE
1     CONTINUE
102   CONTINUE
2     CONTINUE
202   CONTINUE
3     CONTINUE
301   CONTINUE
302   CONTINUE
      IF(C1.LT.0.) C1=0.
      C1=SQRT(C1)
C
  999 RETURN
      END