C+
      SUBROUTINE SAPGEV (NXM, XM, YM, ZM, WEIGHT, INSERT, PMAG, IERR)
C----------------------------------------------------------------------
C
C.        MOMENTUM FIT USING THE QUINTIC SPLINE MODEL.
C.            DOCUMENTATION = NUCL. INSTR. METH.115,
C.            431 (1974)  (H.WIND), REPORT ON OMEGA
C.            EXPERIENCE DD 75.2 (D.TOWNSEND, J.WILSON)
C.            ORIGIN =  CERN LIBRARY X510,
C.            AMENDED D.TOWNSEND(1974)
C.            STATUS = CLEANED-UP FROM FIELD PROVEN CODE
C
C         INPUT PARAMETERS
C             NXM             NUMBER OF MEASURED POINTS
C             XM,YM,ZM        MEASURED POINT COORDINATES
C             WEIGHT          TO BE USED IN FIT
C             INSERT          NUMBER OF MAG.FIELD POINTS TO BE INSERTED
C                             (LAST ELEMENT NOT USED, BUT SET = 0)
C
C         OUPUT PARAMETERS
C             PMAG            FITTED MOMENTUM (APART FROM SCALE FACTOR)
C             PVEC(3)         COMPONENTS OF PMAG
C             A1,A2,D,B1,B2   THE FITTED PARAMETERS
C             ERR(5,5)        ERROR MATRIX FOR THESE
C             IERR            =0 IF O.K., ELSE SETUP ERROR
C
C         LOCAL COMMUNICATION
C             NTOT            TOTAL NUMBER OF POINTS WITH MAG. FIELD
C                             ( = NXM + SUM(INSERT(I))   )
C             X,Y,Z           COORDINATES OF ALL POINTS IN FIT SYSTEM
C             YI,ZI           DY/DX AND DZ/DX ALL POINTS
C             BX,BY,BZ        FIELD COMPONENTS OF ALL POINTS,FIT SYSTEM
C             BXM,BYM,BZM     FIELD COMPONENTS,MEASURED POINTS,
C                             MEASURED SYSTEM
C                             (MAY BE MADE AN INPUT PARAMETER)
C             W11,W12...W33   ORTHONORMAL ROTATION MATRIX BETWEEN
C                             MEASURED AND FIT COORD. SYSTEMS
C             PYII,PZII       SECOND DERIVATIVES ALL POINTS
C             YII,ZII         X AND Y INTEGRATED, FROM DTSPL
C
C-   Created   2-DEC-1993   Alexander Efimov   
C-   Updated   3-FEB-1994   Alexander Efimov   
C
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INTEGER NXM, INSERT(*), IERR
      REAL    XM(*), YM(*), ZM(*), WEIGHT(*), PMAG
      DOUBLE PRECISION  PVEC(3), A1, A2, D, B1, B2, ERR(5,5)
      DOUBLE PRECISION  X(100), Y(100), Z(100), YI(100), ZI(100)
      DOUBLE PRECISION  BXM(100), BYM(100), BZM(100)
      DOUBLE PRECISION  W11, W12, W13, W21, W22, W23, W31, W32, W33
      DOUBLE PRECISION  BX(100), BY(100), BZ(100)
      DOUBLE PRECISION  PYII(100), PZII(100), YII(100), ZII(100)
      DOUBLE PRECISION  UI(100), VI(100), UII(100), VII(100)
C
      INTEGER NTOT, NX, NTO, INCR, ITEM, ITER
      DOUBLE PRECISION  SCC, SYC, SXC, SXY, SXX, SC, SY, SX, SZ, SZCZ
      DOUBLE PRECISION  SXZ, SCZ, SCCZ, SCCSCZ, SYCZCZ, SXCZ
      DOUBLE PRECISION  D1, D2, DD, DY1, DZ1, DY2, DZ2, RY, RZ
      DOUBLE PRECISION  D1Y, D1Z, DX1, DX2, D2Y, D2Z, DX, DY, DZ
      DOUBLE PRECISION  T1, T2, T3, T4, T5, T7, T8, T10, T11, T31, T33
      DOUBLE PRECISION  YPPP, ZPPP, H, QX, QY, QZ
      DOUBLE PRECISION  PV1, PV2, PV3
      DOUBLE PRECISION  A, B, C, SQR, DET, W
      REAL    XYZ(3), BXYZ(3)
      INTEGER I, II, J, JJ, K, KK, KL, IJK
      INTEGER I1, I2, J1, N, N1, N2, K2, K3, NTOT1
C
C         LOCAL PARAMETERS FOR ITERATING AND DIMENSIONING
      INTEGER NITERX, NTOTX
      REAL    SCALE
      SAVE NITERX, NTOTX, SCALE
      REAL THIRD, SIXTH, P7, P8, T24TH
      SAVE THIRD, SIXTH, P7, P8, T24TH
C
      REAL    DXH, DXSQ
      REAL    DX12, DX22
      DOUBLE PRECISION  AIN, XX, YY, ZZ, AA, BB, CC
C
      DATA NITERX/5/, NTOTX/100/, SCALE/2.9979251E-4/
      DATA THIRD /0.333333333333333/
      DATA SIXTH /0.166666666666666/
      DATA P7 /0.7/, P8 /0.8/
      DATA T24TH /0.041666666666666/
C
C         WE DEFINE AN INPRODUCT.
      AIN(XX,YY,ZZ,AA,BB,CC) = XX*AA + YY*BB + ZZ*CC
C
      IERR = 0
      NX = NXM
C         WE NEED AT LEAST THREE POINTS
      IF (NX.LT.3)    GO TO 140
C         AND NO MORE THAN NTOTX
      IF (NX.GT.NTOTX)    GO TO 140
C         ALSO CHECK NTOT
      NTO = NX
      DO I = 2,NX
        NTO = NTO+INSERT(I-1)
      END DO
      NTOT = NTO
      IF (NTO.GT.NTOTX)    GO TO 140
C
C         WE NOW CHANGE THE COORDINATE SYSTEM. IN THE NEW SYSTEM, WE MAY
C         EXPECT X TO BE INCREASING ALONG THE TRACK, Y' TO BE SMALL,
C         AND Z' TO BE VERY SMALL.
C         THE NEW COORDINATE SYSTEM WILL HAVE THE X-AXIS PARALLEL TO THE
C         VECTOR JOINING THE FIRST AND THE LAST POINT ON THE TRACK.
C         THE Y-AXIS WILL HAVE THE DIRECTION OF THE VECTOR JOINING THE
C         LINE SEGMENT BETWEEN THE FIRST AND THE LAST POINT ON THE TRACK
C         WITH A POINT FAR FROM THIS SEGMENT. THE Z-AXIS WILL BE
C         ORTHOGONAL TO THE X- AND Y-AXES, AND THE SYSTEM WILL BE RIGHT-
C         HANDED.
C
C         FIND THE NEW X-AXIS.
      W11 = XM(NX)-XM(1)
      W21 = YM(NX)-YM(1)
      W31 = ZM(NX)-ZM(1)
      D = 1.0/SQRT(W11**2+W21**2+W31**2)
      W11 = W11*D
      W21 = W21*D
      W31 = W31*D
C         FIND THE NEW Y-AXIS
      INCR = NX/7+1
      D = 0.0
      DO 20 I = 2,NX,INCR
        DX = XM(I)-XM(1)
        DY = YM(I)-YM(1)
        DZ = ZM(I)-ZM(1)
        A = AIN(W11,W21,W31,DX,DY,DZ)
C         HERE WE USE X,Y,Z AS TEMPORARY STORAGE SPACE.
        X(I) = DX-A*W11
        Y(I) = DY-A*W21
        Z(I) = DZ-A*W31
        D1 = X(I)**2+Y(I)**2+Z(I)**2
        IF (D1.LT.D)    GO TO 20
        ITEM = I
        D = D1
   20 CONTINUE
      D = 1.0/SQRT(D)
      W12 = X(ITEM)*D
      W22 = Y(ITEM)*D
      W32 = Z(ITEM)*D
C
C         FIND THE NEW Z-AXIS
      W13 = W21*W32-W22*W31
      W23 = W31*W12-W11*W32
      W33 = W11*W22-W12*W21
C
C         COMPUTE NEW COORDINATES X,Y,Z, NEW FIELD-COMPONENTS BX,BY,BZ
C
      INSERT(NX) = 0
      K = 1
      DO I = 1,NX
        QX = XM(I)
        QY = YM(I)
        QZ = ZM(I)
        X(K) = AIN(W11,W21,W31,QX,QY,QZ)
        Y(K) = AIN(W12,W22,W32,QX,QY,QZ)
        Z(K) = AIN(W13,W23,W33,QX,QY,QZ)
        XYZ(1) = XM(I)
        XYZ(2) = YM(I)
        XYZ(3) = ZM(I)
        CALL SAFLD (XYZ, BXYZ)
        BXM(I) = BXYZ(1)
        BYM(I) = BXYZ(2)
        BZM(I) = BXYZ(3)
        BX(K) = AIN(W11,W21,W31,BXM(I),BYM(I),BZM(I))
        BY(K) = AIN(W12,W22,W32,BXM(I),BYM(I),BZM(I))
        BZ(K) = AIN(W13,W23,W33,BXM(I),BYM(I),BZM(I))
        IF (I.NE.1.AND.X(K).LT.X(KK))    GO TO 150
        KK = K
        K = K+INSERT(I)+1
      END DO
C
C         FILL IN THE X-VALUES OF INSERTED POINTS
C         THE CORRESPONDING Y AND Z VALUES AND THE THREE COMPONENTS OF
C         MAGNETIC FIELD WILL BE SET BY SPLFIT
C
      D = 0
      K = 1
      DO 50 I = 2,NX
        KK = K+INSERT(I-1)
        IF (INSERT(I-1).EQ.0)    GO TO 50
        D = (X(KK+1)-X(K))/FLOAT(INSERT(I-1)+1)
        KL = K+1
        DO J = KL,KK
          X(J) = X(J-1)+D
        END DO
   50 K = KK+1
C
C         A CALL TO SPLFIT FOLLOWS.
C         SPLFIT MUST INTERPOLATE TO FIND VALUES FOR Y AND Z AT THE
C         INTERMEDIATE POINTS. IT MUST SET THE DERIVATIVES YI AND ZI AT
C         ALL POINTS, BX,BY AND BZ AT THE INTERPOLATED POINTS.
C         A DIFFERENT INTERPOLATION ROUTINE MAY BE SUBSTITUTED.
C
C==================>      START OF SPLFIT ROUTINE
C
      YII(NTOT) = 0.0
      YII(1) = 0.0
      ZII(NTOT) = 0.0
      ZII(1) = 0.0
      ZI(1) = 1.0
      YI(1) = 1.0
      N1 = NXM-1
      IF (NXM.EQ.3)    GO TO 40
      K2 = 2+INSERT(1)
      D2 = X(K2)-X(1)
      DY2 = Y(K2)-Y(1)
      DZ2 = Z(K2)-Z(1)
      N2 = NXM-2
      I = 1
      I1 = 2+INSERT(1)
C         LOOP AROUND MEASURED POINTS TO GET YI,ZI,YII,ZII
      DO K = 3, NXM
        I2 = I1+INSERT(K-1)+1
        D1 = D2
        D2 = X(I2)-X(I1)
        DD = D1+D2
        DY1 = DY2
        DZ1 = DZ2
        DY2 = Y(I2)-Y(I1)
        DZ2 = Z(I2)-Z(I1)
        RY = 3.0*(DY2/D2-DY1/D1)/DD
        RZ = 3.0*(DZ2/D2-DZ1/D1)/DD
        T1 = 0.5*D1/DD
        T2 = 0.5*D2/DD
        YI(I1) = -T2/(1.0+T1*YI(I))
        ZI(I1) = -T2/(1.0+T1*ZI(I))
        YII(I1) = (RY-T1*YII(I))/(1.0+T1*YI(I))
        ZII(I1) = (RZ-T1*ZII(I))/(1.0+T1*ZI(I))
        I = I1
        I1 = I2
      END DO
C
      NTOT1 = NTOT-1-INSERT(NXM-1)
      YII(NTOT) = YII(NTOT1)/(1.0-YI(NTOT1))
      ZII(NTOT) = ZII(NTOT1)/(1.0-ZI(NTOT1))
      YII(NTOT1) = YII(NTOT)
      ZII(NTOT1) = ZII(NTOT)
      J = NTOT1
      DO I = 2,N2
        J1 = J
        J = J-1-INSERT(NXM-I)
        YII(J) = YI(J)*YII(J1)+YII(J)
        ZII(J) = ZI(J)*ZII(J1)+ZII(J)
      END DO
      YII(1) = YII(K2)
      ZII(1) = ZII(K2)
      I = 1
      I1 = INSERT(1)+2
      DO K = 2,NXM
        DX = X(I1)-X(I)
        DY = Y(I1)-Y(I)
        DZ = Z(I1)-Z(I)
        D1Y = DY/DX
        D1Z = DZ/DX
        YI(I) = D1Y-DX*(THIRD*YII(I)+SIXTH*YII(I1))
        ZI(I) = D1Z-DX*(THIRD*ZII(I)+SIXTH*ZII(I1))
        I = I1
        I1 = I1+INSERT(K)+1
      END DO
      DX = X(NTOT)-X(NTOT1)
      DY = Y(NTOT)-Y(NTOT1)
      DZ = Z(NTOT)-Z(NTOT1)
      D1Y = DY/DX
      D1Z = DZ/DX
      YI(NTOT) = D1Y+DX*0.5*YII(NTOT)
      ZI(NTOT) = D1Z+DX*0.5*ZII(NTOT)
      I = 1
      I1 = 2+INSERT(1)
      GO TO  10
C         FOR ONLY THREE MEASURED POINTS SHORT SECTION
   40 CONTINUE
      K2 = 2+INSERT(1)
      K3 = K2+1+INSERT(2)
      DX1 = X(K2)-X(1)
      DX2 = X(K3)-X(K2)
      D1Y = (Y(K2)-Y(1))/DX1
      D1Z = (Z(K2)-Z(1))/DX1
      D2Y = (Y(K3)-Y(K2))/DX2
      D2Z = (Z(K3)-Z(K2))/DX2
      YII(K2) = 3.0*(D2Y-D1Y)/(DX1+DX2)
      ZII(K2) = 3.0*(D2Z-D1Z)/(DX1+DX2)
      YI(1) = D1Y-SIXTH*DX1*YII(K2)
      ZI(1) = D1Z-SIXTH*DX1*ZII(K2)
      YI(K3) = D2Y+SIXTH*DX2*YII(K2)
      ZI(K3) = D2Z+SIXTH*DX2*ZII(K2)
      YI(K2) = YI(1)+.5*DX1*YII(K2)
      ZI(K2) = ZI(1)+.5*DX1*ZII(K2)
C
C         EXPAND IN TAYLOR SERIES FOR INTERPOLATED POINTS
C
   10 CONTINUE
      II = 1
      DO 70 K = 1,N1
        I = II
        II = II+INSERT(K)+1
        IF (INSERT(K).EQ.0)    GO TO 70
        YPPP = (YII(II)-YII(I))/(X(II)-X(I))
        ZPPP = (ZII(II)-ZII(I))/(X(II)-X(I))
        JJ = INSERT(K)
        DO J = 1,JJ
          H = X(I+J)-X(I)
          YI(I+J) = YI(I)+H*YII(I)+0.5*H**2*YPPP
          ZI(I+J) = ZI(I)+H*ZII(I)+0.5*H**2*ZPPP
          Y(I+J) = Y(I)+H*YI(I)+0.5*H**2*YII(I)+SIXTH*H**3*YPPP
          Z(I+J) = Z(I)+H*ZI(I)+0.5*H**2*ZII(I)+SIXTH*H**3*ZPPP
C         FOR MAG. FIELD HACE TO GO THROUGH MEASUREMENT COORD.SYSTEM
          XYZ(1) = AIN(W11,W12,W13,X(I+J),Y(I+J),Z(I+J))
          XYZ(2) = AIN(W21,W22,W23,X(I+J),Y(I+J),Z(I+J))
          XYZ(3) = AIN(W31,W32,W33,X(I+J),Y(I+J),Z(I+J))
          CALL SAFLD (XYZ, BXYZ)
          QX = BXYZ(1)
          QY = BXYZ(2)
          QZ = BXYZ(3)
          BX(I+J) = AIN(W11,W21,W31,QX,QY,QZ)
          BY(I+J) = AIN(W12,W22,W32,QX,QY,QZ)
          BZ(I+J) = AIN(W13,W23,W33,QX,QY,QZ)
        END DO
   70 CONTINUE
C
C==================>      END OF SPLFIT ROUTINE
C
      ITER = 0
C         BEGIN OF ITERATIONS
   60 ITER = ITER+1
C
C         FIND P.D2Y/DX2 AND P.D2Z/DX2  (PYII AND PZII) AT ALL POINTS.
C         FROM EQUATIONS OF MOTION
C
      DO I = 1,NTO
        SQR = SQRT(1.0+YI(I)**2+ZI(I)**2)
        PYII(I) = SQR*(BX(I)*ZI(I)+BY(I)*YI(I)*ZI(I)-
     &            BZ(I)*(1.0+YI(I)**2))
        PZII(I) = SQR*(BY(I)*(1.0+ZI(I)**2)-BX(I)*YI(I)-
     &            BZ(I)*YI(I)*ZI(I))
      END DO
C
C         WE MAKE A FIT THROUGH PYII AND PZII, TO ESTIMATE YII AND ZII.
C         TO DO THIS WE CALL DTSPL.
C         DTSPL MUST PROVIDE SINGLE AND DOUBLE INTEGRALS OF P.D2Y/DX AND
C         P.D2Z/DX2 (CHOOSING THE INTEGRATION CONSTANTS SO AS TO MAKE
C         THE INTEGRALS ZERO AT THE FIRST POINT. THIS CHOICE IS
C         ARBITRARY)
C
C=====================> START OF DTSPL ROUTINE
C
      N = NTOT
      YII(1) = 0.0
      YI(1) = 0.0
      UII(N) = 0.0
      UII(1) = 0.0
      ZII(1) = 0.0
      ZI(1) = 0.0
      VII(N) = 0.0
      VII(1) = 0.0
      VI(1) = 1.0
      UI(1) = 1.0
      IF (N.EQ.3)    GO TO 30
C         FOR MORE THAN THREE POINTS
      D2 = X(2)-X(1)
      DY2 = PYII(2)-PYII(1)
      DZ2 = PZII(2)-PZII(1)
      N2 = N-2
C         RECURSION FOR P AND Q
      DO I = 1,N2
        D1 = D2
        D2 = X(I+2)-X(I+1)
        DD = D1+D2
        DY1 = DY2
        DZ1 = DZ2
        DY2 = PYII(I+2)-PYII(I+1)
        DZ2 = PZII(I+2)-PZII(I+1)
        RY = 3.0*(DY2/D2-DY1/D1)/DD
        RZ = 3.0*(DZ2/D2-DZ1/D1)/DD
        T2 = 0.5*D2/DD
        T1 = 0.5*D1/DD
        UI(I+1) = -T2/(1.0+T1*UI(I))
        VI(I+1) = -T2/(1.0+T1*VI(I))
        UII(I+1) = (RY-T1*UII(I))/(1.0+T1*UI(I))
        VII(I+1) = (RZ-T1*VII(I))/(1.0+T1*VI(I))
      END DO
C
      UII(N) = UII(N-1)/(1.0-UI(N-1))
      VII(N) = VII(N-1)/(1.0-VI(N-1))
      UII(N-1) = UII(N)
      VII(N-1) = VII(N)
C         RECURSION FOR UII,VII = Y'''',Z''''
      DO I = 2,N2
        J = N-I
        UII(J) = UI(J)*UII(J+1)+UII(J)
        VII(J) = VI(J)*VII(J+1)+VII(J)
      END DO
      UII(1) = UII(2)
      VII(1) = VII(2)
      N1 = N-1
C         RECURSION FOR UI,VI = Y''',Z'''
      DO I = 1,N1
        DX = X(I+1)-X(I)
        DY = PYII(I+1)-PYII(I)
        DZ = PZII(I+1)-PZII(I)
        D1Y = DY/DX
        D1Z = DZ/DX
        UI(I) = D1Y-DX*(THIRD*UII(I)+SIXTH*UII(I+1))
        VI(I) = D1Z-DX*(THIRD*VII(I)+SIXTH*VII(I+1))
        DXH = DX*0.5
        YI(I+1) = DXH*(PYII(I+1)+PYII(I)-THIRD*(UII(I+1)+
     &            UII(I))*DXH**2)+YI(I)
        ZI(I+1) = DXH*(PZII(I+1)+PZII(I)-THIRD*(VII(I+1)+
     &            VII(I))*DXH**2)+ZI(I)
      END DO
      DX = X(N)-X(N-1)
      DY = PYII(N)-PYII(N-1)
      DZ = PZII(N)-PZII(N-1)
      D1Y = DY/DX
      D1Z = DZ/DX
      UI(N) = D1Y+DX*0.5*UII(N)
      VI(N) = D1Z+DX*0.5*VII(N)
C         RECURSION FOR Y'',Z''
      DO I = 1,N1
        DX = X(I+1)-X(I)
        DXSQ = DX*DX*SIXTH
        DXH = 0.5*DX
        YII(I+1) = DXSQ*((PYII(I)+PYII(I)+PYII(I+1))-DXSQ*(P8*UII(I)+
     &            P7*UII(I+1)))+YII(I)+YI(I)*DX
        ZII(I+1) = DXSQ*((PZII(I)+PZII(I)+PZII(I+1))-DXSQ*(P8*VII(I)+
     &            P7*VII(I+1)))+ZII(I)+ZI(I)*DX
      END DO
      GO TO  80
C         THREE-POINT PROCEDURE
   30 CONTINUE
      DX1 = X(2)-X(1)
      DX2 = X(3)-X(2)
      D1Y = (PYII(2)-PYII(1))/DX1
      D1Z = (PZII(2)-PZII(1))/DX1
      D2Y = (PYII(3)-PYII(2))/DX2
      D2Z = (PZII(3)-PZII(2))/DX2
      UII(2) = 3.0*(D2Y-D1Y)/(DX1+DX2)
      VII(2) = 3.0*(D2Z-D1Z)/(DX1+DX2)
      UI(1) = D1Y-SIXTH*DX1*UII(2)
      VI(1) = D1Z-SIXTH*DX1*VII(2)
      UI(3) = D2Y+SIXTH*DX2*UII(2)
      VI(3) = D2Z+SIXTH*DX2*VII(2)
      UI(2) = UI(1)+.5*DX1*UII(2)
      VI(2) = VI(1)+.5*DX1*VII(2)
      DX12 = DX1**2
      YI(2) = DX1*(.5*(PYII(2)+PYII(1))-T24TH*DX12*UII(2))
      ZI(2) = DX1*(.5*(PZII(2)+PZII(1))-T24TH*DX12*VII(2))
      DX22 = DX2**2
      YI(3) = DX2*(.5*(PYII(3)+PYII(2))-T24TH*DX22*UII(2))+YI(2)
      ZI(3) = DX2*(.5*(PZII(3)+PZII(2))-T24TH*DX22*VII(2))+ZI(2)
      DXSQ = SIXTH*DX1**2
      YII(2) = DXSQ*(PYII(1)+PYII(1)+PYII(2)-DXSQ*(P7*UII(2)))
      ZII(2) = DXSQ*(PZII(1)+PZII(1)+PZII(2)-DXSQ*(P7*VII(2)))
      DXSQ = SIXTH*DX2**2
      YII(3) = DXSQ*(PYII(2)+PYII(2)+PYII(3)-DXSQ*(P8*UII(2)))+
     &         YII(2)+YI(2)*DX2
      ZII(3) = DXSQ*(PZII(2)+PZII(2)+PZII(3)-DXSQ*(P8*VII(2)))+
     &         ZII(2)+ZI(2)*DX2
   80 CONTINUE
C=====================> END OF DTSPL ROUTINE
C
C         WE NOW COMPARE OUR INTEGRALS (YII = P*(Y-A2*X-A1) AND ZII =
C         P*(Z-B2*X-B1)) WITH KNOWN VALUES OF Y AND Z. THIS GIVES PMAG
C         (THE QUOTIENT OF MOMENTUM AND CHARGE) AND THE INTEGRATION
C         CONSTANTS.
C
C         WE MAKE A LEAST SQUARES FIT TO FIND P, A1, A2, B1 AND B2.
C
      SCC = 0.0
      SYC = 0.0
      SXC = 0.0
      SXY = 0.0
      SXX = 0.0
      SC = 0.0
      SY = 0.0
      SX = 0.0
      A = 0.0
      SXZ = 0.0
      SZ = 0.0
      SZCZ = 0.0
      SXCZ = 0.0
      SCZ = 0.0
      SCCZ = 0.0
      K = 1
      DO I = 1,NX
        W = WEIGHT(I)
        A = A+W
        SX = SX+X(K)*W
        SY = SY+Y(K)*W
        SC = SC+YII(K)*W
        SXX = SXX+X(K)*X(K)*W
        SXY = SXY+X(K)*Y(K)*W
        SXC = SXC+X(K)*YII(K)*W
        SYC = SYC+Y(K)*YII(K)*W
        SCC = SCC+YII(K)*YII(K)*W
        SCCZ = SCCZ+ZII(K)*ZII(K)*W
        SCZ = SCZ+ZII(K)*W
        SXCZ = SXCZ+X(K)*ZII(K)*W
        SZCZ = SZCZ+Z(K)*ZII(K)*W
        SZ = SZ+Z(K)*W
        SXZ = SXZ+X(K)*Z(K)*W
        K = K+INSERT(I)+1
      END DO
      SCCSCZ = SCC+SCCZ
      SYCZCZ = SYC+SZCZ
C
C
C         THE MATRIX TO BE INVERTED TO MAKE THE LEAST SQUARES FIT IS
C
C             A   SX   SC    0    0
C            SX  SXX  SXC    0    0
C            SC  SXC SCCSCCZ SCZ SXCZ
C             0    0  SCZ    A   SX
C             0    0 SXCZ   SX  SXX
C
C
C          THE UNKNOWNS ARE A1,A2,D,B1,B2
C          THE RIGHT HAND SIDES ARE SY,SXY,SYCZCZ,SZ,SXZ
C
C          THE MEANING OF T1 TO T10 IS CLEAR FROM THE EXPRESSIONS A1=...
C
C
      T1 = SXX*SY-SX*SXY
      T2 = SC*SXX-SXC*SX
      T3 = A*SXX-SX*SX
      T4 = -SX*SY+A*SXY
      T5 = -SC*SX+A*SXC
      T7 = SXX*SZ-SX*SXZ
      T8 = SXX*SCZ-SX*SXCZ
      T10 = -SX*SZ+A*SXZ
      T11 = -SCZ*SX+A*SXCZ
      T31 = 1./T3
      T33 = T3*T3
      D = (SYCZCZ*T33-(SC*T1+SXC*T4)*T3-(SCZ*T7+SXCZ*T10)*T3)
      DET = SCCSCZ*T33-(SC*T2+SXC*T5)*T3-(SCZ*T8+SXCZ*T11)*T3
      IF (ABS(DET) .LT. 1.0E-33) THEN
        IERR = 4
        GO TO 130
      END IF
      DET = 1.0/DET
      D = D*DET
      A1 = (T1-D*T2)*T31
      A2 = (T4-D*T5)*T31
      B1 = (T7-D*T8)*T31
      B2 = (T10-D*T11)*T31
      IF (ABS(D) .LT. 1.0E-33) THEN
        IERR = 5
        GO TO 130
      END IF
      PMAG = SCALE/D
C
C         THE PARAMETERS A1, A2, B1, B2, D, DEFINE A TRACK THAT DOES NOT
C         NECCESARILY GO EXACTLY THROUGH ALL OF THE MEASURED POINTS.
C
      IF (ITER.GE.NITERX)    GO TO 120
C         PREPARE NEXT ITERATION
      K = 1
      DO 100 I = 2,NX
        IF (INSERT(I-1).EQ.0)    GO TO 100
        IJK = INSERT(I-1)
        DO J = 1,IJK
          K = K+1
          Y(K) = D*YII(K)+A2*X(K)+A1
          Z(K) = D*ZII(K)+B2*X(K)+B1
          XYZ(1) = AIN(W11,W12,W13,X(K),Y(K),Z(K))
          XYZ(2) = AIN(W21,W22,W23,X(K),Y(K),Z(K))
          XYZ(3) = AIN(W31,W32,W33,X(K),Y(K),Z(K))
          CALL SAFLD (XYZ, BXYZ)
          QX = BXYZ(1)
          QY = BXYZ(2)
          QZ = BXYZ(3)
          BX(K) = AIN(W11,W21,W31,QX,QY,QZ)
          BY(K) = AIN(W12,W22,W32,QX,QY,QZ)
          BZ(K) = AIN(W13,W23,W33,QX,QY,QZ)
        END DO
  100 K = K+1
      DO K = 1,NTO
        YI(K) = D*YI(K)+A2
        ZI(K) = D*ZI(K)+B2
      END DO
      GO TO  60
C         AFTER LAST ITERATION CONTINUE HERE
  120 CONTINUE
      T1 = D*YI(1)+A2
      T2 = D*ZI(1)+B2
      PV1 = ABS(PMAG)/SQRT(1.0+T1**2+T2**2)
      PV2 = PV1*T1
      PV3 = PV1*T2
      PVEC(1) = AIN(W11,W12,W13,PV1,PV2,PV3)
      PVEC(2) = AIN(W21,W22,W23,PV1,PV2,PV3)
      PVEC(3) = AIN(W31,W32,W33,PV1,PV2,PV3)
C
C         SET UP ERROR MATRIX OF PARAMETERS A1,A2,D,B1 AND B2
C
      ERR(1,1) = SXX*T31+T2*T2*DET
      ERR(1,2) = -SX*T31+T2*T5*DET
      ERR(2,1) = -SX*T31+T2*T5*DET
      ERR(1,3) = -T2*T3*DET
      ERR(3,1) = -T2*T3*DET
      ERR(1,4) = +T2*T8*DET
      ERR(4,1) = +T2*T8*DET
      ERR(1,5) = +T2*T11*DET
      ERR(5,1) = +T2*T11*DET
      ERR(2,2) = A*T31+T5*T5*DET
      ERR(2,3) = -T5*T3*DET
      ERR(3,2) = -T5*T3*DET
      ERR(2,4) = +T5*T8*DET
      ERR(4,2) = +T5*T8*DET
      ERR(2,5) = +T5*T11*DET
      ERR(5,2) = +T5*T11*DET
      ERR(3,3) = T33*DET
      ERR(3,4) = -T3*T8*DET
      ERR(4,3) = -T3*T8*DET
      ERR(3,5) = -T3*T11*DET
      ERR(5,3) = -T3*T11*DET
      ERR(4,4) = SXX*T31+T8*T8*DET
      ERR(4,5) = -SX*T31+T8*T11*DET
      ERR(5,4) = -SX*T31+T8*T11*DET
      ERR(5,5) = A*T31+T11*T11*DET
C
  130 RETURN
C
C         WRONG SETUP CONDITIONS LEAD INTO THIS ERROR SECTION
C             NTOT NOT IN RANGE 3....NTOTX
  140 IERR = 1
      GO TO  130
C             X-COORDINATES NOT IN ASCENDING ORDER AFTER ROTATION
  150 IERR = 3
      GO TO  130
      END
