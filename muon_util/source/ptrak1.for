      SUBROUTINE PTRAK1(IEND,NPL,MINSOZ,NSP,XSP,YSP,ZPL,ROAD,NTR,
     A NSPTR,XSPTR,YSPTR,ZSPTR)
C.        FROM COORDINATES IN TWO PROJECTION,FINDS STRAIGHT LINES.
C.        ALL PLANES ARE ASSUMED PARALLEL, PERPENDICULAR TO Z-AXIS.
C.        SPARKS ARE USED ONLY ONCE.
C.            ORIGIN = V. CHABAUD '79, STATUS = FIELD PROVEN.
C
C         INPUT PARAMETERS
C             IEND      0 NO REQUIREMENT, 1 REQUIRES FIRST PLANE
C                       TO HAVE A HIT, 2 REQUIRES LAST, 3 BOTH
C                       4 last one is vertex. not on track but constained
C             NPL       NUMBER OF PLANES
C             NSP       NUMBER OF SPARKS PER PLANE
C             XSP       COORDINATES OF SPARKS PER PLANE --- VIEW 1
C                       THIS ARRAY WILL GET PARTLY DESTROYED.
C             YSP       COORDINATES OF SPARKS PER PLANE --- VIEW 2
C             ZPL       Z-COORDINATES OF PLANES
C             ROAD(2)   SEARCH ROAD IN EACH VIEW
C
C         OUTPUT PARAMETERS
C             NTR       NUMBER OF TRACKS FOUND
C             NSPTR     NUMBER OF COORDINATES PER TRACK
C             XSPTR     EXTRACTED X-COORDINATES PER TRACK
C             YSPTR     EXTRACTED Y-COORDINATES PER TRACK
C             ZSPTR     EXTRACTED Z-COORDINATES PER TRACK
C
C         LOCALLY DEFINED
C             NTRMX     A DIMENSIONING PARAMETER
C  dh 8/88  change flag for skipping unphysical hits
C  DH 3/90 CHANGE ARRAY FOR ZPL
C  DH 5/90 DO 3D TRACKING
C  DH 6/90 ALLOW REQUIRING FIRST/LAST PLANE (NOT OPTIMIZED)
C  DH 7/90 ALLOW A MISSING HIT IN ONE NON-BEND DUE TO UNPHYSICAL
C-   Updated   7-JAN-1992   Daria Zieminska  introduce ABSDX,DZPL for speed 
C  DH 4/92 redo somewhat how 'missing hit in non-bend' is handled
C  DH 11/92 add IEND=4, larger arrays, allow points used LMAX=4 times
C  DH 4/93 increase MTRMX to 40
C  DH 3/94 decrease MTRMX to 20, LMAX to 3
C  DH 3/95 allow NTRMX=40 if a-layer
      IMPLICIT NONE
      INTEGER NPL,MINSOZ,NSP(13),NTR,NSPTR(40),NTRMX,NFPL,K,MNSI,MNS,
     A MNSTOL,NSCANI,NBMTOL,K1L,K1,K2,K1P1,K2M1,N1,N2,I1,I2,NBM,N,I,
     A NB,NT,IEND,NLAST,JDX(13,40),NLAST2
      REAL XSP(100,13),ZPL(100,13),YSP(100,13),ROAD(2)
      REAL XSPTR(13,40),ZSPTR(13,40),YSPTR(13,40),VX,VY,ROADMIN
      INTEGER NSCAN,MSP(13),NDX(13),NMISS,DZPL2,LSP(100,13),LMAX
      REAL DZI,X1,X2,QX,XCEN,XMIN,X,Y1,Y2,QY,YCEN,Y,ABSDX,DZPL
      DATA NTRMX/40/
      DATA LMAX/4/       ! MAXIMUM NUMBER OF TRACKS A SPARK CAN BE ON
      DATA ROADMIN/5./   ! MINIMUM ROAD IF IN SAME MODULE
      IF(NPL.LE.5) THEN  ! ALLOW MORE IF FEWER PLANES
        NTRMX=40
        LMAX=4
      ELSE
        NTRMX=20
        LMAX=3
      ENDIF
C
      NTR = 0
      NFPL = 0
      NLAST=NPL
      IF(IEND.EQ.4) NLAST=NPL-1
      DO 10 K = 1,NLAST
        DO N=1,13
          JDX(N,K)=0
        ENDDO
        MSP(K) = NSP(K)
        IF (NSP(K).EQ.0)    GO TO 10
        DO I=1,NSP(K)
          LSP(I,K)=0
        ENDDO
        NFPL = NFPL+1
   10 CONTINUE
      IF (NFPL.LT.MINSOZ)    GO TO 120
C LOOK FOR TRACKS WITH HIGHEST NUMBER OF SPARKS FIRST; (MAX OR MAX-1)
      NLAST2=NLAST-1
      IF(NLAST2.LT.MINSOZ) NLAST2=MINSOZ
      DO 110 MNSI = MINSOZ,NLAST2
        MNS = NLAST2+MINSOZ-MNSI
        IF (NFPL.LT.MNS)    GO TO 110
        MNSTOL = MNS
C         LOOP HIGH PIVOT PLANE INDEX
        DO 100 NSCANI = MNS,NLAST
          NSCAN = NLAST+MNS-NSCANI
          NBMTOL = NSCAN-MNSTOL
          K1L = NLAST-NSCAN+1
          IF(IEND.EQ.1.OR.IEND.EQ.3) K1L=1      ! REQUIRE FIRST PLANE
C         LOOP LOW PIVOT PLANE INDEX
          DO 90 K1 = 1,K1L
            IF (MSP(K1).EQ.0)    GO TO 90
            K2 = K1+NSCAN-1
            IF(IEND.EQ.4.AND.K2.EQ.NPL) GO TO 90 ! SKIP 'LAST' PLANE AS VERTEX
            IF(IEND.EQ.2.OR.IEND.EQ.3) THEN    ! REQUIRE LAST PLANE
              IF(K2.NE.NPL) GO TO 90
            ENDIF
            IF (MSP(K2).EQ.0)    GO TO 90
            K1P1 = K1+1
            K2M1 = K2-1
            N1 = NSP(K1)
            N2 = NSP(K2)
C         LOOP OVER SPARKS IN LOW PIVOT PLANE
            DO 80 I1 = 1,N1
              X1 = XSP(I1,K1)
              Y1 = YSP(I1,K1)
              IF (ABS(X1).GE.80000.)    GO TO 80
              IF (ABS(Y1).GE.80000.)    GO TO 80
C         LOOP OVER SPARKS IN HIGH PIVOT PLANE
              DO 70 I2 = 1,N2
                X2 = XSP(I2,K2)
                IF (ABS(X2).GE.80000.)    GO TO 70
                Y2 = YSP(I2,K2)
                IF (ABS(Y2).GE.80000.)    GO TO 70
                DZI=1./(ZPL(I2,K2)-ZPL(I1,K1))
                QX = (X2-X1)*DZI
                QY = (Y2-Y1)*DZI
                IF(IEND.EQ.4) THEN ! REQUIRE SOME PROJECT TO VERTEX
CC     MOSTLY IN BEND VIEW
                  VX=X2+QX*(ZPL(1,NPL)-ZPL(I2,K2))-XSP(1,NPL)
                  VY=Y2+QY*(ZPL(1,NPL)-ZPL(I2,K2))-YSP(1,NPL)
                  IF(ABS(VX).GT.8.*ROAD(1)) GO TO 70  ! about 50 cm
                  IF(ABS(VY).GT.3.*ROAD(2)) GO TO 70  ! about 300 cm
                ENDIF
                DO K=1,NLAST
                  NDX(K)=0
                ENDDO
C         LOOP OVER ALL PLANES BETWEEN PIVOTS
                NBM = 0
                NT=0
                NMISS=0
cxxx                DO 50 K = K1P1,K2       
                DO 50 K = 1,NLAST
                  IF(K.EQ.K1.OR.K.EQ.K2) GO TO 50
                  IF (MSP(K).EQ.0)    GO TO 40
                  N = NSP(K)
C         LOOP OVER SPARKS IN NON-PIVOT PLANE
C     FIND MINIMUM X WITH Y WITHIN ROADS
                  XMIN=99999.
                  DO 20 I = 1,N
                    X = XSP(I,K)
                    IF(ABS(X).GT.80000.) GO TO 20
                    Y = YSP(I,K)
                    DZPL=ZPL(I,K)-ZPL(I1,K1)
                    DZPL2=ZPL(I,K)-ZPL(I2,K2)
                    XCEN=X1+QX*DZPL
                    YCEN=Y1+QY*DZPL 
                    ABSDX=ABS(X-XCEN)
CCCC  if in same module as end point, use min road
                    IF(ABS(DZPL).LE.20..AND.ABSDX.GT.ROADMIN.AND.
     &              ABS(X-X1).GT.ROADMIN) GO TO 20
                    IF(ABS(DZPL2).LE.20..AND.ABSDX.GT.ROADMIN.AND.
     &              ABS(X-X2).GT.ROADMIN) GO TO 20
                    IF(ABS(Y).LT.80000.) THEN
                      IF(ABS(Y-YCEN).LT.ROAD(2).AND.ABSDX.LT.
     &                ROAD(1).AND.ABSDX.LT.XMIN) THEN
                        XMIN=ABSDX 
                        NDX(K)=I
                      ENDIF
                    ENDIF
   20             CONTINUE
                  IF(XMIN.LT.ROAD(1)) THEN
                    NT=NT+1
                    GO TO  50
                  ENDIF
CC   DID NOT FIND HIT IN THIS PLANE; TRY AGAIN ALLOWING NON-PHYSICAL DELTAT
CC   COULD ALSO IGNORE TIME DIVISION
                  IF(NMISS.LE.1) THEN   ! ONE SINGLE VIEW HIT
                  DO 21 I = 1,N
                    X = XSP(I,K)
                    IF(ABS(X).GT.80000.) GO TO 21
                    Y = YSP(I,K)
                    DZPL=ZPL(I,K)-ZPL(I1,K1)
                    XCEN=X1+QX*DZPL
                    ABSDX=ABS(X-XCEN)
CCCC  if in same module as end point, use min road
                    IF(ABS(DZPL).LE.20..AND.ABSDX.GT.ROADMIN.AND.
     &              ABS(X-X1).GT.ROADMIN) GO TO 21
                    IF(ABS(DZPL2).LE.20..AND.ABSDX.GT.ROADMIN.AND.
     &              ABS(X-X2).GT.ROADMIN) GO TO 21
C                    IF(ABS(Y).GT.80000.) THEN
CCCCCCCCC     ALLOW UNPHYSICAL DELTAT HIT
                    IF(ABSDX.LT.ROAD(1).AND.ABSDX.LT.XMIN) THEN
                      XMIN=ABSDX 
                      NDX(K)=-I    ! MINUS TO FLAG NO DELTA T
                    ENDIF
C                  ENDIF
   21             CONTINUE
                  IF(XMIN.LT.ROAD(1)) THEN
                    NT=NT+1
                    NMISS=NMISS+1
                    GO TO  50
                  ENDIF
                  ENDIF
   40             NBM = NBM+1
C         TEST FOR EFFICIENT STOP OF SEARCH
cxxx                 IF (NBM.GT.NBMTOL) GO TO 70 ! NBM IS NUMBER OF MISSED PLANES
                  IF (NBM.GT.NLAST-MNS) GO TO 70 ! 
   50           CONTINUE
C
                IF(NT+2.LT.MNS) GO TO 70
                NDX(K1) = I1
                NDX(K2) = I2
C    SEE IF TRACK ALREADY FOUND
                IF(NTR.GE.1) THEN
                  DO 53 N=1,NTR
                    DO K=1,NLAST
                      IF(NDX(K).NE.JDX(K,N)) GO TO 53
                    ENDDO
                    GO TO 70   ! SAME TRACK
   53             CONTINUE 
                ENDIF
C         TRACK HAS BEEN FOUND, EXTRACT INFORMATION
                NTR = NTR+1
                NB = 0
                DO 60 K = 1,NLAST
                  JDX(K,NTR)=NDX(K)
                  I = IABS(NDX(K))
                  IF (I.EQ.0)    GO TO 60
                  NB = NB+1
                  XSPTR(NB,NTR) = XSP(I,K)
                  YSPTR(NB,NTR) = YSP(I,K)
CCC    DELTA T NOT USED
                  IF(NDX(K).LT.0) YSPTR(NB,NTR)=-999999.
                  ZSPTR(NB,NTR) = ZPL(I,K)
                  LSP(I,K)=LSP(I,K)+1      ! NUMBER OF TIMES HITS IS USED
C         WIPE OUT USED SPARKS AND EMPTY PLANES
C    DON'T WIPE OUT FIRST/LAST PLANES IF REQUIRED ON TRACK
                  IF(LSP(I,K).GE.LMAX) THEN
                    IF(IEND.EQ.1.AND.K.EQ.1) THEN
                    ELSE IF(IEND.EQ.2.AND.K.EQ.NPL) THEN
                    ELSE IF(IEND.EQ.3.AND.K.EQ.1) THEN
                    ELSE IF(IEND.EQ.3.AND.K.EQ.NPL) THEN
                    ELSE
                      MSP(K) = MSP(K)-1
                      IF (MSP(K).EQ.0)    NFPL = NFPL-1
                      XSP(I,K) = -100000.
                      YSP(I,K) = -100000.
                    ENDIF
                  ENDIF
   60           CONTINUE
                NSPTR(NTR) = NB
C         PROTECT STORE (RECALL IS POSSIBLE)
                IF (NTR.EQ.NTRMX)    GO TO 120
C
                IF (NFPL.LT.MNS)    GO TO 110
                GO TO  80
C
   70         CONTINUE
   80       CONTINUE
   90     CONTINUE
  100   CONTINUE
  110 CONTINUE
C
  120 RETURN
      END
