      SUBROUTINE FTRHI2
C----------------------------------------------------------------------
C
C    Purpose and Methods : Booking and filling more histograms for
C                          FTRAKS package.
C
C-   Created   1-OCT-1990   Jeffrey Bantly  taken from FTRHIS
C-   Updated  17-JUN-1991   Susan K. Blessing  Change size of SCONT array.
C-   Updated  17-SEP-1991   Susan K. Blessing  Change size of (I)QTRAK
C-    to accomodate theta and phi errors and two spare words.
C
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$PARAMS:FDPARA.PARAMS'
C
      INTEGER H,U,QU,S,W,UB,MAXHIS,PREV_W,LAYER
      PARAMETER( MAXHIS = 97 )
      INTEGER IER,ERR,IQTRAK(26),NID(MAXHIS),ID,ICONT(10)
      INTEGER ITRA,NFDC,LOC,IUSER,LTMP,NTRK
      INTEGER LKFDCT, LKFDTH, LOGCHA, LYR, MODULE, NSEG, LSEG
      INTEGER LKFDCH,NHITS,IADD,WPOS,I,NLYR,LSEGN(0:2)
      INTEGER LKFTSC,NSW0HT,IHIT
      INTEGER NHITQD,NHITSC,ISECT
      INTEGER LKFTQD,LKFPSC,LKFXDA
      INTEGER ISECTD,ISEG,IOFSET,FRATIO,NHIT,IOFSET2,IPTR(0:15)
      INTEGER IWIRE,WIR_USED(0:15),MAXWIR,HITTOT,IPNTR,ISKIP
      INTEGER LKFXSC,DAPTR,IHITD,SECTR,WIRNEW,IZ,ITRA2,ICONT2(10),NN
      INTEGER IQTRAK2(26),LADDER(0:2)
      INTEGER GZFSEG,GZFDCT,GZFDCH,GZFTSC,GZFTQD,GZFTDA,GZFPSC,GZFPDA
      INTEGER NZBANK
      EXTERNAL GZFSEG,GZFDCT,GZFDCH,GZFTSC,GZFTQD,GZFTDA,GZFPSC,GZFPDA
      EXTERNAL NZBANK
C
      REAL SHIST(4,MAXHIS),CONT(26),QFSEC(3,34),Z0(2)
      REAL CHI,QTRAK(26),XPOS,YPOS,ZPOS,BESDIF,RESID,ANS
      REAL FSECTD,FIADD,FNHITS,FLRWIR,FIHIT,SCONT(62)
      REAL ZVERTX, ZERROR, ZDIF, ZISAJT, PULAREA, PULSUM, PULWID
      REAL X0,Y0,X02,Y02,ZPTEN,ZZERO,DXDZ,DYDZ
      REAL XO,YO,A1,B1,C1,A2,B2,C2,ZEND,ZSTEP,NUMER,DENOM
      REAL LINEDIST,POINTDIST,FDCDIST,BESTZ,MINPDIST
      REAL CONT2(26),QFSEC2(3,34),QTRAK2(26),DXDZ2,DYDZ2
      REAL SLPDIFX,SLPDIFY,POSDIFX,POSDIFY,ZBEST2,ZBEST3
      REAL XBEST2,XBEST3,YBEST2,YBEST3,MINPDIST2
      EQUIVALENCE (ISECTD,FSECTD),(IADD,FIADD),(NHITS,FNHITS)
      EQUIVALENCE(QTRAK,IQTRAK),(QTRAK2,IQTRAK2)
C
      LOGICAL FIRST,FDC,FDVERT,FVONLY,LKFXDAN,LKFXSCN
C
      CHARACTER*34 NAME(1:40),NAME2(41:MAXHIS)
      CHARACTER*32 STRING
C
      SAVE FIRST,IUSER,NAME,NAME2,SHIST,Z0
      DATA FIRST/.TRUE./
      DATA IUSER/8/
      DATA NAME/' Track Theta SWire 0',' Track Theta DLine 8,9',
     &          ' Track Theta SWire 7',' Track Theta SWire 1to6',
     &          ' Track Phi SWire 0',' Track Phi SWire 15',
     &          ' Track Phi SWire 1to14',' Track All Wires',
     &          ' Sum PH all inner SW on Track',
     &          ' On Segmt Theta SWire 0',' On Segmt Theta SWire 7',
     &          ' On Segmt Theta SWire 1to6',' On Segmt Phi SWire 0',
     &          ' On Segmt Phi SWire 15',' On Segmt Phi SWire 1to14',
     &          ' Off Segmt Theta SWire 0',' Off Segmt Theta SWire 7',
     &          ' Off Segmt Theta SWire 1to6',' Off Segmt Phi SWire 0',
     &          ' Off Segmt Phi SWire 15',' Off Segmt Phi SWire 1to14',
     &          ' On Segmt All Wires',' Off Segmt All Wires',
     &          ' All Theta SW1to7 Sec w hits',
     &          ' All Theta SW0 Sec w hits',
     &          ' All Theta DL Sec w hits',' All Phi SW Sec w hits',
     &          ' Segmt Theta SW0 PH',' Segmt Theta SW7 PH',
     &          ' Segmt Theta SW1to6 PH',' Segmt Phi SW0 PH',
     &          ' Segmt Phi SW15 PH',' Segmt Phi SW1to14 PH',
     &          ' Segmt Theta SW0 PW',' Segmt Theta SW7 PW',
     &          ' Segmt Theta SW1to6 PW',' Segmt Phi SW0 PW',
     &          ' Segmt Phi SW15 PW',' Segmt Phi SW1to14 PW',
     &          ' Segmt Theta SW0 PA VS PW'/
      DATA NAME2 /' Segmt Theta SW0 PA VS PW',
     &          ' Segmt Theta SW7 PA VS PW',' Segmt Theta SW7 PA VS PW',
     &          ' Segmt Theta SW1to6 PA VS PW',
     &          ' Segmt Theta SW1to6 PA VS PW',
     &          ' Segmt Phi SW0 PA VS PW',' Segmt Phi SW0 PA VS PW',
     &          ' Segmt Phi SW15 PA VS PW',' Segmt Phi SW15 PA VS PW',
     &          ' Segmt Phi SW1to14 PA VS PW',
     &          ' Segmt Phi SW1to14 PA VS PW',
     &          ' XAVG at Z=-10',' YAVG at Z=-10',
     &          ' XAVG at Z=-5',' YAVG at Z=-5',
     &          ' XAVG at Z=0',' YAVG at Z=0',
     &          ' XAVG at Z=+5',' YAVG at Z=+5',
     &          ' XAVG at Z=+10',' YAVG at Z=+10',
     &          ' XAVG at Z=+15',' YAVG at Z=+15',
     &          ' Point Dis vs Line Dis',' Point Dis vs Line Dis',
     &          ' Point Dis2 vs Line Dis',' Point Dis2 vs Line Dis',
     &          ' Point Dis vs Point Dis2',' Point Dis vs Point Dis2',
     &          ' Best Z vs Best Z 2',' Best Z vs Best Z 2',
     &          ' Best Z All',' Best Z 2 All',
     &          ' Best Z at Wide Ang 10cm',' Best Z 2 at Wide Ang 10cm',
     &          ' WidAng10 Mdis vs BestZ',' WidAng10 Mdis vs BestZ',
     &          ' WidAng25 Mdis vs BestZ',' WidAng25 Mdis vs BestZ',
     &          ' WidAng10 Mdis vs BestZ',' WidAng10 Mdis vs BestZ',
     &          ' WidAng25 Mdis vs BestZ',' WidAng25 Mdis vs BestZ',
     &          ' WidAng10 Mdis vs BestZ2',' WidAng10 Mdis vs BestZ2',
     &          ' WidAng25 Mdis vs BestZ2',' WidAng25 Mdis vs BestZ2',
     &          ' WidAng10 Mdis vs BestZ2',' WidAng10 Mdis vs BestZ2',
     &          ' WidAng25 Mdis vs BestZ2',' WidAng25 Mdis vs BestZ2',
     &          ' Ioniz per Th SW0 hit',' Ioniz per Th SW1to6 hit',
     &          ' Ioniz per Th SW7 hit',' Ioniz per Ph SW0 hit',
     &          ' Ioniz per Ph SW1to14 hit',' Ioniz per Ph SW15 hit'/
C
C----------------------------------------------------------------------
C
      CALL DHDIR('FTRAKS_RCP','HBOOK_DIRECTORY',ERR,' ')
      IF(ERR.NE.0) THEN
        CALL ERRMSG('FTRAKS','FTRHI2',' ERROR SETTING HBK DIR','W')
      ENDIF
C
      IF (FIRST) THEN    ! Book histograms
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET('MXHALF',MXHALF,ERR)
        CALL EZGET('MXUNIT',MXUNIT,ERR)
        CALL EZGET('MXQUAD',MXQUAD,ERR)
        CALL EZGET('MXSECT',MXSECT,ERR)
        CALL EZGET('MXSECP',MXSECP,ERR)
        CALL EZGET('SHIST',SHIST(1,1),ERR)
        CALL EZGET('Z0',Z0,ERR)
        CALL EZRSET
        IOFSET=200
        DO 100 ID=1,40
          IF (SHIST(1,ID).EQ.1.) THEN
            CALL HBOOK1(ID+IOFSET,NAME(ID),
     &        NINT(SHIST(2,ID)),SHIST(3,ID),SHIST(4,ID),0.)
          ENDIF
          IF (SHIST(1,ID).EQ.2.) THEN
            CALL HBOOK2(ID+IOFSET,NAME(ID),NINT(SHIST(2,ID)),
     &                  SHIST(3,ID),SHIST(4,ID),NINT(SHIST(2,ID+1)),
     &                  SHIST(3,ID+1),SHIST(4,ID+1),0.)
          ENDIF
  100   CONTINUE
        DO 101 ID=41,MAXHIS
          IF (SHIST(1,ID).EQ.1.) THEN
            CALL HBOOK1(ID+IOFSET,NAME2(ID),
     &        NINT(SHIST(2,ID)),SHIST(3,ID),SHIST(4,ID),0.)
          ENDIF
          IF (SHIST(1,ID).EQ.2.) THEN
            CALL HBOOK2(ID+IOFSET,NAME2(ID),NINT(SHIST(2,ID)),
     &                  SHIST(3,ID),SHIST(4,ID),NINT(SHIST(2,ID+1)),
     &                  SHIST(3,ID+1),SHIST(4,ID+1),0.)
          ENDIF
  101   CONTINUE
        FIRST = .FALSE.
      END IF
C
      CALL GTFTRH(ICONT)
      NFDC=ICONT(2)
      IF (NFDC.LT. 0) GOTO 999
C
      NTRK=0
C      IF (NFDC.EQ.0) GOTO 200
      DO 201 ITRA=1,NFDC
        CALL GTFDCT(ITRA,CONT,QFSEC,LADDER)
        CALL UCOPY(CONT,QTRAK,26)
        IF(IQTRAK(2).LE.0) GOTO 201
        NTRK=NTRK+1
C        CHI=SQRT(2*QTRAK(19))-SQRT(2*(FLOAT(IQTRAK(2))-2.)-1.)
C
        W=-1
        PULSUM = 0.
        HITTOT = 0
        DO 202 IHIT=1,IQTRAK(2)
          CALL UCOPY(QFSEC(1,IHIT),IADD,1)
          CALL UCOPY(QFSEC(2,IHIT),IPNTR,1)
          CALL UCOPY(QFSEC(3,IHIT),RESID,1)
          PREV_W=W
          CALL FCODER(IADD/2,H,U,QU,S,W,UB,1)
          IF(IQTRAK(2).LE.25) GOTO 206          ! want 3-seg track
          IF(IHIT.EQ.1 .AND. NFDC.GT.1) THEN
            DXDZ=QTRAK(7)
            DYDZ=QTRAK(8)
C
            DO 203 IZ=0,5
              ZZERO=5.0*IZ-10.
              X0=QTRAK(4)+DXDZ*(ZZERO-Z0(H+1))
              Y0=QTRAK(5)+DYDZ*(ZZERO-Z0(H+1))
              IF(SHIST(1,52+IZ*2).EQ.1.)
     &               CALL HFF1(52+IZ*2+IOFSET,NID(52+IZ*2),X0,1.)
              IF(SHIST(1,53+IZ*2).EQ.1.)
     &               CALL HFF1(53+IZ*2+IOFSET,NID(53+IZ*2),Y0,1.)
  203       CONTINUE
C
            ZZERO=0.0
            X0=QTRAK(4)+DXDZ*(ZZERO-Z0(H+1))
            Y0=QTRAK(5)+DYDZ*(ZZERO-Z0(H+1))
C
            DO 204 ITRA2=ITRA+1,NFDC
              CALL GTFDCT(ITRA2,CONT2,QFSEC2,LADDER)
              CALL UCOPY(CONT2,QTRAK2,26)
              IF(IQTRAK2(2).LE.0) GOTO 204
              IF(IQTRAK2(2).LE.25) GOTO 204     ! want 3-seg track
              DXDZ2=QTRAK2(7)
              DYDZ2=QTRAK2(8)
              X02=QTRAK2(4)+DXDZ2*(ZZERO-Z0(H+1))
              Y02=QTRAK2(5)+DYDZ2*(ZZERO-Z0(H+1))
C
              A1=1.
              B1=(QTRAK(5)-Y0)/(QTRAK(4)-X0)
              C1=(Z0(H+1)-ZZERO)/(QTRAK(4)-X0)
              A2=1.
              B2=(QTRAK2(5)-Y02)/(QTRAK2(4)-X02)
              C2=(Z0(H+1)-ZZERO)/(QTRAK2(4)-X02)
              NUMER=(X02-X0)*(B1*C2-B2*C1) + (Y02-Y0)*(C1*A2-C2*A1)
     &              + (0.*(A1*B2-A2*B1))  ! both at Z=ZZERO
              DENOM=(B1*C2-B2*C1)**2. + (C1*A2-C2*A1)**2. +
     &              (A1*B2-A2*B1)**2.
              IF(DENOM.LE. 0.0) GOTO 204
              DENOM=SQRT(DENOM)
              LINEDIST=ABS(NUMER/DENOM)
              ZSTEP=(Z0(H+1)/ABS(Z0(H+1)))*(-1.)
              ZEND=(-1.)*Z0(H+1)
              FDCDIST=SQRT( (QTRAK(4)-QTRAK2(4))**2. +
     &                                    (QTRAK(5)-QTRAK2(5))**2. )
              BESTZ=140.*( Z0(H+1)/ABS(Z0(H+1)) )
              MINPDIST = 999.
C
              DO 205 ZPOS=Z0(H+1),ZEND,ZSTEP
                XO=QTRAK(4)+DXDZ*(ZPOS-Z0(H+1)) ! Point is on line 1
                YO=QTRAK(5)+DYDZ*(ZPOS-Z0(H+1))
                NUMER=( ((YO-Y02)*C2-(ZPOS-ZZERO)*B2)**2. +
     &                  ((ZPOS-ZZERO)*A2-(XO-X02)*C2)**2. +
     &                  ((XO-X02)*B2-(YO-Y02)*A2)**2. )
                DENOM=A2**2. + B2**2. + C2**2.
                POINTDIST=ABS(SQRT(NUMER/DENOM))
                IF(POINTDIST.LT. MINPDIST) THEN
                  BESTZ = ZPOS
                  MINPDIST=POINTDIST
                ENDIF
  205         CONTINUE
C
              DO 207 ZPOS=BESTZ-0.6,BESTZ+0.6,0.1
                XO=QTRAK(4)+DXDZ*(ZPOS-Z0(H+1)) ! Point is on line 1
                YO=QTRAK(5)+DYDZ*(ZPOS-Z0(H+1))
                NUMER=( ((YO-Y02)*C2-(ZPOS-ZZERO)*B2)**2. +
     &                  ((ZPOS-ZZERO)*A2-(XO-X02)*C2)**2. +
     &                  ((XO-X02)*B2-(YO-Y02)*A2)**2. )
                DENOM=A2**2. + B2**2. + C2**2.
                POINTDIST=ABS(SQRT(NUMER/DENOM))
                IF(POINTDIST.LT. MINPDIST) THEN
                  BESTZ = ZPOS
                  MINPDIST=POINTDIST
                ENDIF
  207         CONTINUE
C
              SLPDIFX=QTRAK(7) - QTRAK2(7)
              SLPDIFY=QTRAK(8) - QTRAK2(8)
              POSDIFX=QTRAK(4)-QTRAK2(4)-(SLPDIFX*Z0(H+1))
              POSDIFY=QTRAK(5)-QTRAK2(5)-(SLPDIFY*Z0(H+1))
              ZBEST2=(-1.)*(SLPDIFX*POSDIFX+SLPDIFY*POSDIFY)/
     &                   (SLPDIFX**2. + SLPDIFY**2.)
              IF(ABS(ZBEST2).GT.ABS(Z0(H+1)) ) ZBEST2=Z0(H+1)
              XBEST2=QTRAK(4)+(ZBEST2-Z0(H+1))*QTRAK(7)
              YBEST2=QTRAK(5)+(ZBEST2-Z0(H+1))*QTRAK(8)
              XBEST3=QTRAK2(4)+(ZBEST2-Z0(H+1))*QTRAK2(7)
              YBEST3=QTRAK2(5)+(ZBEST2-Z0(H+1))*QTRAK2(8)
              MINPDIST2=SQRT( (XBEST2-XBEST3)**2. + (YBEST2-YBEST3)**2.)
C
              IF(SHIST(1,64).EQ. 2.) CALL HFF2(64+IOFSET,
     &             NID(64),LINEDIST,MINPDIST,1.)
              IF(SHIST(1,66).EQ. 2.) CALL HFF2(66+IOFSET,
     &             NID(66),LINEDIST,MINPDIST2,1.)
              IF(SHIST(1,68).EQ. 2.) CALL HFF2(68+IOFSET,
     &             NID(68),MINPDIST2,MINPDIST,1.)
              IF(SHIST(1,70).EQ. 2.) CALL HFF2(70+IOFSET,
     &             NID(70),BESTZ,ZBEST2,1.)
C
              IF(ABS(MINPDIST).GT. 149.) MINPDIST=149.
              IF(ABS(MINPDIST2).GT. 149.) MINPDIST2=149.
              IF(SHIST(1,72).EQ. 1.) CALL HFF1(72+IOFSET,NID(72),
     &                     BESTZ,1.)
              IF(SHIST(1,73).EQ. 1.) CALL HFF1(73+IOFSET,NID(73),
     &                     ZBEST2,1.)
              IF(ABS(BESTZ).LE. 50. .AND. MINPDIST .LE. 2.) THEN
                IF(SHIST(1,74).EQ. 1.) CALL HFF1(74+IOFSET,NID(74),
     &                     BESTZ,1.)
              ENDIF
              IF(ABS(ZBEST2).LE. 50. .AND. MINPDIST2 .LE. 2.) THEN
                IF(SHIST(1,75).EQ. 1.) CALL HFF1(75+IOFSET,NID(75),
     &                     ZBEST2,1.)
              ENDIF
C
c              IF(FDCDIST.GT.10. .AND. MINPDIST.LT.(FDCDIST-5.)) THEN
              IF(FDCDIST.GT.10.) THEN
                IF(SHIST(1,76).EQ. 2.) CALL HFF2(76+IOFSET,
     &             NID(76),BESTZ,MINPDIST,1.)
              ENDIF
              IF(FDCDIST.GT.25.) THEN
                IF(SHIST(1,78).EQ. 2.) CALL HFF2(78+IOFSET,
     &             NID(78),BESTZ,MINPDIST,1.)
              ENDIF
              IF(FDCDIST.GT.10. .AND. MINPDIST.LT.(FDCDIST-1.)) THEN
                IF(SHIST(1,80).EQ. 2.) CALL HFF2(80+IOFSET,
     &             NID(80),BESTZ,MINPDIST,1.)
              ENDIF
              IF(FDCDIST.GT.25. .AND. MINPDIST.LT.(FDCDIST-1.)) THEN
                IF(SHIST(1,82).EQ. 2.) CALL HFF2(82+IOFSET,
     &             NID(82),BESTZ,MINPDIST,1.)
              ENDIF
C
              IF(FDCDIST.GT.10.) THEN
                IF(SHIST(1,84).EQ. 2.) CALL HFF2(84+IOFSET,
     &             NID(84),ZBEST2,MINPDIST2,1.)
              ENDIF
              IF(FDCDIST.GT.25.) THEN
                IF(SHIST(1,86).EQ. 2.) CALL HFF2(86+IOFSET,
     &             NID(86),ZBEST2,MINPDIST2,1.)
              ENDIF
C              IF(FDCDIST.GT.10. .AND. LINEDIST.LT.(FDCDIST-5.)) THEN
              IF(FDCDIST.GT.10. .AND. MINPDIST2.LT.(FDCDIST-1.)) THEN
                IF(SHIST(1,88).EQ. 2.) CALL HFF2(88+IOFSET,
     &             NID(88),ZBEST2,MINPDIST2,1.)
              ENDIF
              IF(FDCDIST.GT.25. .AND. MINPDIST2.LT.(FDCDIST-1.)) THEN
                IF(SHIST(1,90).EQ. 2.) CALL HFF2(90+IOFSET,
     &             NID(90),ZBEST2,MINPDIST2,1.)
              ENDIF
C
  204       CONTINUE                    ! End of ITRA2 loop
          ENDIF
  206     CONTINUE
          NHIT=0
          PULAREA=0.
          IF(U.EQ.0) THEN
            LKFXSC=GZFTSC(H,QU,S)
            IF(LKFXSC.LE. 0) THEN
              WRITE(*,*) ' FTRHI2 - LKFTSC is bad value=',LKFXSC
              GOTO 202
            ENDIF
            LKFXDA=GZFTDA(H,QU,S)
            IF(LKFXDA.LE. 0) THEN
              WRITE(*,*) ' FTRHI2 - LKFTDA is bad value=',LKFXDA
              GOTO 202
            ENDIF
            IF(W.EQ.PREV_W .AND. W.EQ.0) THEN
              NHIT=-1
              NHIT=(IQ(LKFXDA+4+8)+IQ(LKFXDA+4+9))/2
              ID=2
            ELSEIF(W.EQ.0) THEN
              NHIT=IQ(LKFXSC+4+W)
              ID=1
              PULAREA=Q(LKFXSC+IPNTR+7)
              IF(NFDC.LE.2 .AND. SHIST(1,92).EQ.1.) 
     &                    CALL HFF1(92+IOFSET,NID(92),PULAREA,1.)
              PULAREA=0.0
            ELSEIF(W.EQ.7) THEN
              NHIT=IQ(LKFXSC+4+W)
              ID=3
              PULAREA=Q(LKFXSC+IPNTR+7)
              IF(NFDC.LE.2 .AND. SHIST(1,94).EQ.1.) 
     &                    CALL HFF1(94+IOFSET,NID(94),PULAREA,1.)
              PULAREA=0.0
            ELSEIF(W.GE.1 .AND. W.LE.6) THEN
              NHIT=IQ(LKFXSC+4+W)
              PULAREA=Q(LKFXSC+IPNTR+7)
              DAPTR=IQ(LKFXSC+IPNTR+10)
              IF(DAPTR.GT. 0 .AND. DAPTR.LT. 10000) THEN
                PULWID=Q(LKFXDA+DAPTR+4)
              ENDIF
              HITTOT = HITTOT + 1
              ID=4
              IF(NFDC.LE.2 .AND. SHIST(1,93).EQ.1.) 
     &                    CALL HFF1(93+IOFSET,NID(93),PULAREA,1.)
            ENDIF
          ELSEIF(U.EQ.1) THEN
            LKFXDA=GZFPDA(H,S)
            IF(LKFXDA.LE. 0) THEN
              WRITE(*,*) ' FTRHI2 - LKFPDA is bad value=',LKFXDA
              GOTO 202
            ENDIF
            LKFXSC=GZFPSC(H,S)
            IF(LKFXSC.LE. 0) THEN
              WRITE(*,*) ' FTRHI2 - LKFPSC is bad value=',LKFXSC
              GOTO 202
            ENDIF
            IF(W.EQ.0) THEN
              ID=5
              NHIT=IQ(LKFXSC+4+W)
              PULAREA=Q(LKFXSC+IPNTR+7)
              IF(NFDC.LE.2 .AND. SHIST(1,95).EQ.1.) 
     &                    CALL HFF1(95+IOFSET,NID(95),PULAREA,1.)
              PULAREA=0.0
            ELSEIF(W.EQ.15) THEN
              ID=6
              NHIT=IQ(LKFXSC+4+W)
              PULAREA=Q(LKFXSC+IPNTR+7)
              IF(NFDC.LE.2 .AND. SHIST(1,97).EQ.1.) 
     &                    CALL HFF1(97+IOFSET,NID(97),PULAREA,1.)
              PULAREA=0.0
            ELSEIF(W.GE.1 .AND. W.LE.14) THEN
              ID=7
              NHIT=IQ(LKFXSC+4+W)
              PULAREA=Q(LKFXSC+IPNTR+7)
              DAPTR=IQ(LKFXSC+IPNTR+10)
              IF(DAPTR.GT. 0 .AND. DAPTR.LT. 10000) THEN
                PULWID=Q(LKFXDA+DAPTR+4)
              ENDIF
              HITTOT = HITTOT + 1
              IF(NFDC.LE.2 .AND. SHIST(1,96).EQ.1.) 
     &                    CALL HFF1(96+IOFSET,NID(96),PULAREA,1.)
            ENDIF
          ENDIF
          IF(NHIT.GT.0) THEN
            IF(SHIST(1,ID).EQ.1.)
     &               CALL HFF1(ID+IOFSET,NID(ID),FLOAT(NHIT),1.)
            IF(SHIST(1,8).EQ.1.)
     &               CALL HFF1(8+IOFSET,NID(8),FLOAT(NHIT),1.)
          ENDIF
          PULSUM = PULSUM + PULAREA
  202   CONTINUE                        ! End of IHIT loop
        IF(HITTOT.LE. 1) HITTOT = 1
        PULSUM = PULSUM / FLOAT(HITTOT)
        IF(SHIST(1,9).EQ.1.)
     &               CALL HFF1(9+IOFSET,NID(9),PULSUM,1.)
  201 CONTINUE                          ! End of ITRA loop
C
  200 CONTINUE                          ! End of tracks
C
      DO 301 H=0,MXHALF
        DO 302 LYR=0,2
          NSEG=0
          MODULE=H*3 + LYR
          LSEG=GZFSEG(H,LYR)
          IF(LSEG.GT.0) NSEG=NZBANK(IXCOM,LSEG)
          DO 303 ISEG=1,NSEG
            CALL GTFSEG(MODULE,ISEG,SCONT)
            FSECTD=SCONT(1)
            FIADD=SCONT(2)
            FNHITS=SCONT(3)
            CALL FCODER(IADD,H,U,QU,S,W,UB,1)
            IF(U.EQ.0) THEN
              LKFXDA=GZFTDA(H,QU,S)
              LKFXSC=GZFTSC(H,QU,S)
              MAXWIR=7
            ELSEIF(U.EQ.1) THEN
              LKFXDA=GZFPDA(H,S)
              LKFXSC=GZFPSC(H,S)
              MAXWIR=15
            ELSE
              LKFXDA=0
              LKFXSC=0
              MAXWIR=-1
            ENDIF
            IF(LKFXSC.LE. 0) THEN
              WRITE(*,*) ' FTRHI2 - LKFXSC is bad=',LKFXSC
              GOTO 303
            ENDIF
            IF(LKFXDA.LE. 0) THEN
              WRITE(*,*) ' FTRHI2 - LKFXDA is bad=',LKFXDA
              GOTO 303
            ENDIF
            IHITD=50
            IF(ABS(ISECTD).GT.50) THEN
              IF(ABS(ISECTD).GT.50) GOTO 303
              IHITD=ISECTD/1000
              SECTR=S+(ISECTD/ABS(ISECTD))
              IF(U.EQ.0) THEN
                LKFXDAN=GZFTDA(H,QU,SECTR)
                LKFXSCN=GZFTSC(H,QU,SECTR)
              ELSEIF(U.EQ.1) THEN
                LKFXDAN=GZFPDA(H,SECTR)
                LKFXSCN=GZFPSC(H,SECTR)
              ENDIF
              IF(LKFXSCN.LE. 0) THEN
                WRITE(*,*) ' FTRHI2 - LKFXSCN is bad=',LKFXSCN
                GOTO 303
              ENDIF
              IF(LKFXDAN.LE. 0) THEN
                WRITE(*,*) ' FTRHI2 - LKFXDAN is bad=',LKFXDAN
                GOTO 303
              ENDIF
            ENDIF
            CALL VZERO(WIR_USED(0),16)
            CALL VZERO(IPTR(0),16)
            ISKIP=8
            IF(U.EQ.1) ISKIP=16
            WIRNEW=50
            DO 304 IHIT=1, NHITS
              IWIRE=INT(SCONT(3+IHIT)/2.)
C              LR   =INT(SCONT(3+IHIT))-IWIRE*2
              IPTR(IWIRE) =INT(SCONT(3+IHIT+ISKIP))
C              RESID=SCONT(3+IHIT+IOFSET2)
              WIR_USED(IWIRE)=1
              IF(IHIT.EQ.ABS(IHITD)) WIRNEW=IWIRE
  304       CONTINUE
            DO 305 IWIRE=0,MAXWIR
              IF(IWIRE.GE.WIRNEW) THEN
                LKFXDA=LKFXDAN
                LKFXSC=LKFXSCN
              ENDIF
              IOFSET2 = 0
              IF( WIR_USED(IWIRE) .EQ. 0) IOFSET2=6
              NHIT=IQ(LKFXSC+4+IWIRE)
              PULAREA = 0.
              PULWID  = 0.
              ID = 0
              IF(U.EQ.0) THEN
                IF(IWIRE.EQ.0) THEN
                  ID=10+IOFSET2
                ELSEIF(IWIRE.EQ.7) THEN
                  ID=11+IOFSET2
                ELSEIF(IWIRE.GE.1 .AND. IWIRE.LE.7) THEN
                  ID=12+IOFSET2
                ENDIF
              ELSEIF(U.EQ.1) THEN
                IF(IWIRE.EQ.0) THEN
                  ID=13+IOFSET2
                ELSEIF(IWIRE.EQ.15) THEN
                  ID=14+IOFSET2
                ELSEIF(IWIRE.GE.1 .AND. IWIRE.LE.14) THEN
                  ID=15+IOFSET2
                ENDIF
              ENDIF
              PULAREA=Q(LKFXSC+IPTR(IWIRE)+7)
              DAPTR  =IQ(LKFXSC+IPTR(IWIRE)+10)
              PULWID =Q(LKFXDA+DAPTR+4)
              IF(ID.GE.10 .AND. ID.LE.21) THEN
                IF(SHIST(1,ID).EQ.1.) CALL HFF1(ID+IOFSET,NID(ID),
     &                                                 FLOAT(NHIT),1.)
                IF(SHIST(1,22+IOFSET2/6).EQ.1.)
     &            CALL HFF1(22+IOFSET+IOFSET2/6,NID(22+IOFSET2/6),
     &                                                 FLOAT(NHIT),1.)
              ENDIF
              IF(ID.GE.10 .AND. ID.LE.15) THEN
                IF(SHIST(1,ID+18).EQ. 1.) CALL HFF1(ID+18+IOFSET,
     &                                          NID(ID+18),PULAREA,1.)
                IF(SHIST(1,ID+24).EQ. 1.) CALL HFF1(ID+24+IOFSET,
     &                                          NID(ID+24),PULWID,1.)
                IF(SHIST(1,ID+30+ID-10).EQ. 2.)
     &                          CALL HFF2(ID+30+IOFSET+(ID-10),
     &                         NID(ID+30+ID-10),PULAREA,PULWID,1.)
              ENDIF
  305       CONTINUE
  303     CONTINUE
  302   CONTINUE
  301 CONTINUE
C
      DO 401 H=0,MXHALF
        DO 402 U=0,MXUNIT
          IF(U.EQ.0) THEN
            DO 403 QU=0,MXQUAD
              DO 404 S=0,MXSECT
                LKFXSC=GZFTSC(H,QU,S)
                IF(LKFXSC.GT.5) THEN
                  IF(SHIST(1,25).EQ. 1.) CALL HFF1(25+IOFSET,NID(25),
     &                                   FLOAT(IQ(LKFXSC+4)),1.)
                  DO 406 W=1,7
                    NHIT=IQ(LKFXSC+4+W)
                    IF(SHIST(1,24).EQ. 1.) CALL HFF1(24+IOFSET,NID(24),
     &                                   FLOAT(NHIT),1.)
  406             CONTINUE
                  IF(SHIST(1,26).EQ. 1.) CALL HFF1(26+IOFSET,NID(26),
     &                                   FLOAT(IQ(LKFXSC+4+8)),1.)
                  IF(SHIST(1,26).EQ. 1.) CALL HFF1(26+IOFSET,NID(26),
     &                                   FLOAT(IQ(LKFXSC+4+9)),1.)
                ENDIF
  404         CONTINUE
  403       CONTINUE
          ELSEIF(U.EQ.1) THEN
            DO 405 S=0,MXSECP
              LKFXSC=GZFPSC(H,S)
              IF(LKFXSC.GT.5) THEN
                DO 407 W=0,15
                  NHIT=IQ(LKFXSC+4+W)
                  IF(SHIST(1,27).EQ. 1.) CALL HFF1(27+IOFSET,NID(27),
     &                                   FLOAT(NHIT),1.)
  407           CONTINUE
              ENDIF
  405       CONTINUE
          ENDIF
  402   CONTINUE
  401 CONTINUE
C
C  Done.
C
C-------------------------------------------------------------------------
  999 RETURN
      END
