      SUBROUTINE FVTRAK 
C------------------------------------------------------------------------
C 
C  Make Central Detector (CD) tracks by linking FDC and VTX tracks.
C  Loop over FDC tracks and match VTX tracks by comparing their parameters 
C  (phi and ends in x,y view and theta); flag used tracks.
C  Store CD tracks in banks ZTRK. Increment number of CD tracks in bank
C  ZTRH (header for CD tracks).
C                               
C-   Created  xx-MAR-1989   Daria Zieminska  
C-   Updated  14-MAR-1990   Jeffrey Bantly  use any path 
C-   Updated  17-SEP-1991   Susan K. Blessing  Use correct values of 
C-    theta, error on theta, error on phi from FDCT bank.
C
C------------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LFTRH,GZFTRH,LVTRH
      INCLUDE 'D0$PARAMS:ZTRAKS.PARAMS'                             
      INCLUDE 'D0$LINKS:IZVTRH.LINK/LIST'                             
      INCLUDE 'D0$LINKS:IZFTRH.LINK/LIST'                             
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'                             
      INTEGER NZ,STAT,IBIT,JBIT,LOCF,LOCV,IER 
      INTEGER GZVTXT,GZFDCT,LZTRH,GZZTRH,LZTRK,NVMAX
      PARAMETER (NVMAX=1000)
      INTEGER GOODF(NVMAX),GOODVF(NVMAX),GOODV(NVMAX)
      INTEGER NV,IV,NF,IF,NZTRAK,NRPHI
      REAL PHIF,EPHIF,PHIV(NVMAX),EPHIV
      REAL XGF,YGF,XGV,YGV,THETAF,DELTHE(NVMAX)
      REAL DELPHI,QUALIT,IONIZ 
      REAL PT,ETHETF,ETHETV,ERRTHE,DIFTHE
      REAL DXDZF,DYDZF,DXDZV,DYDZV,DXDZF2,DYDZF2,THETAV
      LOGICAL FIRST,SAME,ENDSEG,MATCH(NVMAX)
      INCLUDE 'D0$INC:PI.DEF'
      DATA PT/1./
      DATA FIRST/.TRUE./
C------------------------------------------------------------------------
      IF (FIRST) THEN
        CALL EZPICK('ZTRAKS_RCP')
        CALL EZGET('DIFTHE',DIFTHE,IER)
        CALL EZGET('ERRTHE',ERRTHE,IER)
        FIRST=.FALSE.
      END IF
      NZ=0
      LZTRH=GZZTRH() 
      IF (LZTRH.EQ.0) GO TO 999
      LFTRH=LQ(LZTRH-IZFTRH) 
      IF (LFTRH.EQ.0) GO TO 999
      NF=IQ(LFTRH+2)
      IF (NF.LE.0) GO TO 999
      LVTRH=LQ(LZTRH-IZVTRH)            
      IF (LVTRH.EQ.0) GO TO 999
      NV=IQ(LVTRH+2)
      NZTRAK=IQ(LZTRH+2)     
      DO 200 IF=1,NF
        LOCF=GZFDCT(IF) 
        IF (LOCF.EQ.0) GO TO 200
        STAT=IQ(LOCF)
        IF (JBIT(STAT,IUSED).EQ.1) GO TO 200
        PHIF=Q(LOCF+6)
        EPHIF = Q(LOCF+23)
        XGF=Q(LOCF+4)
        YGF=Q(LOCF+5)
C        DXDZF=Q(LOCF+7)
C        DYDZF=Q(LOCF+8)
C        DXDZF2=DXDZF**2
C        DYDZF2=DYDZF**2
C        ETHETF=DXDZF2*Q(LOCF+16)+DXDZF*DYDZF*Q(LOCF+17)+
C     &   DYDZF2*Q(LOCF+18)
C        ETHETF=SQRT(ETHETF/(DXDZF2+DYDZF2))
C        THETAF=ATAN2(DYDZF,SIN(PHIF))
        THETAF = Q(LOCF+22)
        ETHETF = Q(LOCF+24)
        IF (THETAF.LT.0.) THETAF=THETAF+PI
        NRPHI=0
        DO 300 IV=1,NV
          LOCV=GZVTXT(IV) 
          IF (LOCV.EQ.0) GO TO 300
          STAT=IQ(LOCV)
          MATCH(IV)=0
          IF (JBIT(STAT,IUSED).EQ.1) GO TO 300
          PHIV(IV)=Q(LOCV+6)
          EPHIV=Q(LOCV+16)
          XGV=Q(LOCV+7)
          YGV=Q(LOCV+8)
          CALL FVRPHI(PHIV(IV),PHIF,PT,XGF,YGF,XGV,YGV,MATCH(IV))
          IF (MATCH(IV)) THEN   
            NRPHI=NRPHI+1
            GOODVF(IV)=0
            GOODF(IV)=0
            GOODV(IV)=0
            ETHETV=Q(LOCV+18)
            IF (ETHETF.GT.ERRTHE.AND.ETHETV.GT.ERRTHE) THEN
              MATCH(IV)=.FALSE.
            ELSE IF (ETHETF.LT.ERRTHE.AND.ETHETV.LT.ERRTHE) THEN
              THETAV=Q(LOCV+9)
              DELTHE(IV)=ABS(THETAF-THETAV)
              MATCH(IV)=DELTHE(IV).LT.DIFTHE 
              IF (MATCH(IV)) GOODVF(IV)=1
            ELSE IF (ETHETF.LT.ERRTHE.AND.ETHETV.GT.ERRTHE) THEN
              MATCH(IV)=.TRUE.
              GOODF(IV)=1
              GOODV(IV)=0
            ELSE IF (ETHETV.LT.ERRTHE.AND.ETHETF.GT.ERRTHE) THEN
              MATCH(IV)=.TRUE.
              GOODF(IV)=0
              GOODV(IV)=1
            END IF
          END IF
  300   CONTINUE
        DO 400 IV=1,NV
          IF (.NOT.MATCH(IV)) GO TO 400
          IF (NRPHI.GT.1.AND.GOODVF(IV).EQ.0) GO TO 400
          QUALIT = ABS(PHIV(IV)-PHIF)   ! Quality of track (temp. def.)
C            QUALIT=QUALIT/SQRT(EPHIC**2+EPHIV**2)
          CALL BKZTRK(LZTRK)
          IF (NRPHI.EQ.1) IQ(LZTRK)=IBSET(IQ(LZTRK),12)
          IF (GOODVF(IV).EQ.1) IQ(LZTRK)=IBSET(IQ(LZTRK),11)
          IF (GOODF(IV).EQ.1) IQ(LZTRK)=IBSET(IQ(LZTRK),10)
          IF (GOODV(IV).EQ.1) IQ(LZTRK)=IBSET(IQ(LZTRK),9)
          LOCF=GZFDCT(IF) 
          IF (LOCF.EQ.0) GO TO 400
          LOCV=GZVTXT(IV) 
          IF (LOCV.EQ.0) GO TO 400
          CALL MZFLAG(IXCOM,LOCF,IUSED,' ')
          CALL MZFLAG(IXCOM,LOCV,IUSED,' ')
          LQ(LZTRK-8)=LOCF
          LQ(LZTRK-6)=LOCV
          LZTRH=GZZTRH()
          IF (LZTRH.EQ.0) GO TO 400
          IQ(LZTRH+2)=IQ(LZTRH+2)+1
          NZ=NZ+1
          IQ(LZTRK-5)=NZ+NZTRAK 
          IQ(LZTRK+2)=IV
          IQ(LZTRK+4)=IF
          Q(LZTRK+6)=QUALIT 
          IF (GOODVF(IV).EQ.1) THEN
            Q(LZTRK+8)=DELTHE(IV)
          ELSE
            Q(LZTRK+8)=99. 
          END IF
  400   CONTINUE
  200 CONTINUE
  999 RETURN
      END
