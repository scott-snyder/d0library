C DEC/CMS REPLACEMENT HISTORY, Element MUFIT3.FOR
C *1    20-MAY-1991 12:48:14 DIESBURG "Terminated loops on unique statements"
C DEC/CMS REPLACEMENT HISTORY, Element MUFIT3.FOR
C DEC/CMS REPLACEMENT HISTORY, Element MUFIT3.FOR
C *1    20-JUN-1989 16:10:22 HEDIN " "
C DEC/CMS REPLACEMENT HISTORY, Element MUFIT3.FOR
      SUBROUTINE MUFIT3(VER,SW,SZ,SZ2,SX,SX2,SXZ,N,IV,K1,K2,Z,XV,
     A  X1,Z1,SL1,C1,IS)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC     LOOP OVER PAD COMBINATIONS; USE OPTIMIZED CALCULATION
CC     ASSUMES ALL POINTS ARE GOOD. HAS MAXIMUM OF 10 POINTS ON FIT
CC     MIMIMUM OF 3. THIS DOES COMBINED PAD AND DELTA T WITH DELTA T
CC     COMING FROM INPUT SUMS
CC
CC     INPUT: N          = NUMBER OF POINTS (GE.3)
CC            IV         = GUESS AT INITIAL VERNIER SOLUTION
CC            K1         = LOWER LOOP INDEX
CC            K2         = UPPER LOOP INDEX
CC            Z          = POSITION BETWEEN PLANES
CC            XV         = PAD SOLUTIONS
CC            SW,ETC     = SUM OF 'OTHER' POINTS ON FIT (COULD BE 0)
CC            VER        = SIZE OF PAD PATTERN
CC     OUTPUT: IS        = PAD SOLUTION
CC             X1        = FITTED POSITION (ALONG WIRE DIRECTION)
CC             Z1        = CENTER OF GRAVITY BETWEEN PLANES FROM FIT
CC            SL1        = SLOPE DX/DZ
CC             C1        = SQRT(CHI**2/(N-2))
CC
CC     HEDIN 5/11/89   PATTERNED AFTER MUFIT2
CC    DH 8/90 CHANGE ARRAY SIZES
CC      sew 10/90 fix max hits to 10
CC     SEW/DH  4/91 make intermediate cuts on chisq
CC     M. Diesburg 5/91 terminated loops on unique statements
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT NONE
      INTEGER N,NFIT,I,NMAX,NLIM
      PARAMETER (NFIT=20)
      PARAMETER (NMAX=10)
      REAL X1,Z1,SL1,C1,VER
      REAL Z(NFIT),XV(2,NFIT),SWA,SZA,SZ2A,ZQ,SWB(NFIT),SZB(NFIT)
     &  ,SZ2B(NFIT),ZQB(NFIT)
      REAL X0A,SLA,ZA,CHIMX,CHI,VXG,VSL,X(NFIT)
      REAL SW,SX,SZ,SX2,SZ2,SXZ,SX4,SX5,SX6,SX7,SX8,SX9,SX10,
     A SX24,SX25,SX26,SX27,SX28,SX29,SX210,SXZ4,SXZ5,SXZ6,SXZ7,
     A SXZ8,SXZ9,SXZ10,SX3,SX23,SXZ3,SX22,SXZ2,SSX2,CHIMAX
      REAL SX11,SX12,SX13,SX14,SX15,SX211,SX212,SX213,SX214,SX215,
     1 SXZ11,SXZ12,SXZ13,SXZ14,SXZ15,FACTOR
      PARAMETER (FACTOR=2.0)
      INTEGER IS(NFIT),IV(NFIT),K1(NFIT),K2(NFIT)
      INTEGER II,I1,I2,I3,I4,I5,I6,I7,I8,I9,I10,I11,I12,I13,I14,I15
      INTEGER NA(6),NB(6)
      DATA NA/-2,-2,-1,-1,0,0/
      DATA NB/1,2,1,2,1,2/
C
C
      IF(N.LE.1.OR.N.GT.NFIT) THEN   ! TOO FEW OR TOO MANY POINTS
        GO TO 999
      ENDIF
C
      SZA=SZ
      SZ2A=SZ2
      NLIM=MIN(N,NMAX)
      DO I=1,NLIM
        SZA=SZA+Z(I)
        SZ2A=SZ2A+Z(I)**2
        IF (I.EQ.1) THEN
          SZB(I)=SZ+Z(I)
          SZ2B(I)=SZ2+Z(I)**2
          SWB(I)=SW+1.
        ELSE
          SZB(I)=SZB(I-1)+Z(I)
          SZ2B(I)=SZ2B(I-1)+Z(I)**2
          SWB(I)=SWB(I-1)+1.
        ENDIF
        ZQB(I)=SZ2B(I)-SZB(I)**2/SWB(I)
      ENDDO
      SWA=SW+FLOAT(NLIM)
      ZQ=SZ2A-SZA**2/SWA
C    LOOP OVER ALL PAD SOLUTIONS; ASSUME CAN BE ON 3 DIFFERENT
C    WAVELENGTHS PLUS TWO POSSIBLE SOLUTIONS
C
      CHIMX=100000.
      CHIMAX=CHIMX
      C1=CHIMX
C
      DO 350 I1=K1(1),K2(1)
        X(1)=VER*(IV(1)+NA(I1)) + XV(NB(I1),1)
        DO 325 I2=K1(2),K2(2)
          IF(NLIM.GE.2) THEN
            X(2)=VER*(IV(2)+NA(I2)) + XV(NB(I2),2)
C        SSX2=SX+X(1)/WT(1)**2+X(2)/WT(2)**2
C        SX22=SX2+X(1)**2/WT(1)**2+X(2)**2/WT(2)**2
C        SXZ2=SXZ+X(1)*Z(1)/WT(1)**2+X(2)*Z(2)/WT(2)**2
            SSX2=SX+X(1)+X(2)
            SX22=SX2+X(1)**2+X(2)**2
            SXZ2=SXZ+X(1)*Z(1)+X(2)*Z(2)
          ENDIF
          DO 300 I3=K1(3),K2(3)
C
            IF(NLIM.GE.3) THEN
C        SX3=SSX2+X(3)/WT(3)**2
C        SX23=SX22+X(3)**2/WT(3)**2
C        SXZ3=SXZ2+X(3)*Z(4)/WT(3)**2
              X(3)=VER*(IV(3)+NA(I3)) + XV(NB(I3),3)
              SX3=SSX2+X(3)
              SX23=SX22+X(3)**2
              SXZ3=SXZ2+X(3)*Z(3)
              CHI=SX23-SX3**2/SWB(3)-(SXZ3-SX3*SZB(3)/SWB(3))**2/ZQB(3)
              IF (CHI.GT.FACTOR*CHIMX) GO TO 300
            ENDIF
C
            DO 225 I4=K1(4),K2(4)
              IF(NLIM.GE.4) THEN
C        SX4=SX3+X(4)/WT(4)**2
C        SX24=SX23+X(4)**2/WT(4)**2
C        SXZ4=SXZ3+X(4)*Z(4)/WT(4)**2
                X(4)=VER*(IV(4)+NA(I4)) + XV(NB(I4),4)
                SX4=SX3+X(4)
                SX24=SX23+X(4)**2
                SXZ4=SXZ3+X(4)*Z(4)
                CHI=SX24-SX4**2/SWB(4)-(SXZ4-SX4*SZB(4)/SWB(4))**2/
     &            ZQB(4)
                IF (CHI.GT.FACTOR*CHIMX) GO TO 225
              ENDIF
              DO 200 I5=K1(5),K2(5)
                IF(NLIM.GE.5) THEN
C        SX5=SX4+X(5)/WT(5)**2
C        SX25=SX24+X(5)**2/WT(5)**2
C        SXZ5=SXZ4+X(5)*Z(5)/WT(5)**2
                  X(5)=VER*(IV(5)+NA(I5)) + XV(NB(I5),5)
                  SX5=SX4+X(5)
                  SX25=SX24+X(5)**2
                  SXZ5=SXZ4+X(5)*Z(5)
                  CHI=SX25-SX5**2/SWB(5)-(SXZ5-SX5*SZB(5)/SWB(5))**2/
     &              ZQB(5)
                  IF (CHI.GT.FACTOR*CHIMX) GO TO 200
                ENDIF
                DO 125 I6=K1(6),K2(6)
                  IF(NLIM.GE.6) THEN
C        SX6=SX5+X(6)/WT(6)**2
C        SX26=SX25+X(6)**2/WT(6)**2
C        SXZ6=SXZ5+X(6)*Z(6)/WT(6)**2
                    X(6)=VER*(IV(6)+NA(I6)) + XV(NB(I6),6)
                    SX6=SX5+X(6)
                    SX26=SX25+X(6)**2
                    SXZ6=SXZ5+X(6)*Z(6)
                    CHI=SX26-SX6**2/SWB(6)-(SXZ6-SX6*SZB(6)/SWB(6))**2/
     &                ZQB(6)
                    IF (CHI.GT.FACTOR*CHIMX) GO TO 125
                  ENDIF
                  DO 100 I7=K1(7),K2(7)
                    IF(NLIM.GE.7) THEN
C        SX7=SX6+X(7)/WT(7)**2
C        SX27=SX26+X(7)**2/WT(7)**2
C        SXZ7=SXZ6+X(7)*Z(7)/WT(7)**2
                      X(7)=VER*(IV(7)+NA(I7)) + XV(NB(I7),7)
                      SX7=SX6+X(7)
                      SX27=SX26+X(7)**2
                      SXZ7=SXZ6+X(7)*Z(7)
                      CHI=SX27-SX7**2/SWB(7)-(SXZ7-SX7*SZB(7)/SWB(7))**
     &                  2/ZQB(7)
                      IF (CHI.GT.FACTOR*CHIMX) GO TO 100
                    ENDIF
                    DO 450 I8=K1(8),K2(8)
                      IF(NLIM.GE.8) THEN
C        SX8=SX7+X(8)/WT(8)**2
C        SX28=SX27+X(8)**2/WT(8)**2
C        SXZ8=SXZ7+X(8)*Z(8)/WT(8)**2
                        X(8)=VER*(IV(8)+NA(I8)) + XV(NB(I8),8)
                        SX8=SX7+X(8)
                        SX28=SX27+X(8)**2
                        SXZ8=SXZ7+X(8)*Z(8)
                        CHI=SX28-SX8**2/SWB(8)-(SXZ8-SX8*SZB(8)/
     &                    SWB(8))**2/ZQB(8)
                        IF (CHI.GT.FACTOR*CHIMX) GO TO 450
                      ENDIF
                      DO 425 I9=K1(9),K2(9)
                        IF(NLIM.GE.9) THEN
C        SX9=SX8+X(9)/WT(9)**2
C        SX29=SX28+X(9)**2/WT(9)**2
C        SXZ9=SXZ8+X(9)*Z(9)/WT(9)**2
                          X(9)=VER*(IV(9)+NA(I9)) + XV(NB(I9),9)
                          SX9=SX8+X(9)
                          SX29=SX28+X(9)**2
                          SXZ9=SXZ8+X(9)*Z(9)
                          CHI=SX29-SX9**2/SWB(9)-(SXZ9-SX9*SZB(9)/
     &                      SWB(9))**2/ZQB(9)
                          IF (CHI.GT.FACTOR*CHIMX) GO TO 425
                        ENDIF
                        DO 400 I10=K1(10),K2(10)
C      IF(N.GE.10)  SX10=SX9+X(10)/WT(10)**2
C      IF(N.GE.10)  SX210=SX29+X(10)**2/WT(10)**2
C      IF(N.GE.10)  SXZ10=SXZ9+X(10)*Z(10)/WT(10)**2
                          IF(NLIM.GE.10) THEN
                            X(10)=VER*(IV(10)+NA(I10)) + XV(NB(I10),10)
                            SX10=SX9+X(10)
                            SX210=SX29+X(10)**2
                            SXZ10=SXZ9+X(10)*Z(10)
                            CHI=SX210-SX10**2/SWB(10)-(SXZ10-SX10*
     &                        SZB(10)/SWB(10))**2/ZQB(10)
                            IF (CHI.GT.FACTOR*CHIMX) GO TO 400
                          ENDIF
C
C                          IF(NLIM.EQ.2) THEN
C                            CHI=SX22-SSX2**2/SWA-(SXZ2-SSX2*SZA/SWA)**2/
C     &                        ZQ
C                          ELSE IF(NLIM.EQ.3) THEN
C                            CHI=SX23-SX3**2/SWA-(SXZ3-SX3*SZA/SWA)**2/ZQ
C                          ELSE IF(NLIM.EQ.4) THEN
C                            CHI=SX24-SX4**2/SWA-(SXZ4-SX4*SZA/SWA)**2/ZQ
C                          ELSE IF(NLIM.EQ.5) THEN
C                            CHI=SX25-SX5**2/SWA-(SXZ5-SX5*SZA/SWA)**2/ZQ
C                          ELSE IF(NLIM.EQ.6) THEN
C                            CHI=SX26-SX6**2/SWA-(SXZ6-SX6*SZA/SWA)**2/ZQ
C                          ELSE IF(NLIM.EQ.7) THEN
C                            CHI=SX27-SX7**2/SWA-(SXZ7-SX7*SZA/SWA)**2/ZQ
C                          ELSE IF(NLIM.EQ.8) THEN
C                            CHI=SX28-SX8**2/SWA-(SXZ8-SX8*SZA/SWA)**2/ZQ
C                          ELSE IF(NLIM.EQ.9) THEN
C                            CHI=SX29-SX9**2/SWA-(SXZ9-SX9*SZA/SWA)**2/ZQ
C                          ELSE IF(NLIM.EQ.10) THEN
C                            CHI=SX210-SX10**2/SWA-(SXZ10-SX10*SZA/SWA)**
C     &                        2/ZQ
C                          ENDIF
C
                                    IF(CHI.LT.-10.) GO TO 400
                                    IF(CHI.GT.CHIMX) GO TO 400
                                    CHIMX=CHI
                                    C1=CHI
                                    IF(NLIM.EQ.2) THEN
                                      CALL MUSLIN(SWA,SSX2,SZA,SX22,
     &                                 SZ2A,SXZ2,X0A,ZA,SLA,VXG,VSL,CHI)
                                    ELSE IF(NLIM.EQ.3) THEN
                                      CALL MUSLIN(SWA,SX3,SZA,SX23,SZ2A,
     &                                 SXZ3,X0A,ZA,SLA,VXG,VSL,CHI)
                                    ELSE IF(NLIM.EQ.4) THEN
                                      CALL MUSLIN(SWA,SX4,SZA,SX24,SZ2A,
     &                                 SXZ4,X0A,ZA,SLA,VXG,VSL,CHI)
                                    ELSE IF(NLIM.EQ.5) THEN
                                      CALL MUSLIN(SWA,SX5,SZA,SX25,SZ2A,
     &                                 SXZ5,X0A,ZA,SLA,VXG,VSL,CHI)
                                    ELSE IF(NLIM.EQ.6) THEN
                                      CALL MUSLIN(SWA,SX6,SZA,SX26,SZ2A,
     &                                 SXZ6,X0A,ZA,SLA,VXG,VSL,CHI)
                                    ELSE IF(NLIM.EQ.7) THEN
                                      CALL MUSLIN(SWA,SX7,SZA,SX27,SZ2A,
     &                                 SXZ7,X0A,ZA,SLA,VXG,VSL,CHI)
                                    ELSE IF(NLIM.EQ.8) THEN
                                      CALL MUSLIN(SWA,SX8,SZA,SX28,SZ2A,
     &                                  SXZ8,X0A,ZA,SLA,VXG,VSL,CHI)
                                    ELSE IF(NLIM.EQ.9) THEN
                                      CALL MUSLIN(SWA,SX9,SZA,SX29,SZ2A,
     &                                  SXZ9,X0A,ZA,SLA,VXG,VSL,CHI)
                                    ELSE IF(NLIM.EQ.10) THEN
                                      CALL MUSLIN(SWA,SX10,SZA,SX210,
     &                                SZ2A,SXZ10,X0A,ZA,SLA,VXG,VSL,CHI)
                                    ENDIF
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
  400                   CONTINUE
  425                 CONTINUE
  450               CONTINUE
  100             CONTINUE
  125           CONTINUE
  200         CONTINUE
  225       CONTINUE
  300     CONTINUE
  325   CONTINUE
  350 CONTINUE
      IF(C1.LT.-.1.OR.C1.GT.10000.) GO TO 999
      IF(C1.LT.0.) C1=0.
      DO I=1,NLIM
        II=IS(I)
        IS(I)=IV(I)+NA(II)+1
        IF(NB(II).EQ.1) IS(I)=-IS(I)
      ENDDO
C
      RETURN
  999 CONTINUE
      C1=-1.
      SL1=10000.
      Z1=-100000.
      X1=-999999.
      RETURN
      END
