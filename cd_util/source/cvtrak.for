      SUBROUTINE CVTRAK 
C------------------------------------------------------------------------
C 
C  Make Central Detector (CD) tracks by linking CDC and VTX tracks.
C  Loop over CDC tracks and match VTX tracks by comparing their parameters 
C  (phi and ends in x,y view and theta); flag used tracks.
C  Store CD tracks in banks ZTRK. Increment number of CD tracks in bank
C  ZTRH (header for CD tracks).
C                               
C  Daria Zieminska  Nov 1988
C-   Updated  16-FEB-1990   Qizhong Li-Demarteau  call CVMTCH for new phi
C-                                                cut and new theta cut 
C-   Updated  10-JUL-1991   Qizhong Li-Demarteau  added EZRSET and EZERROR
C
C------------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LDTRH,LVTRH,NVMAX
      PARAMETER (NVMAX=1000)
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'                             
      INCLUDE 'D0$PARAMS:ZTRAKS.PARAMS'                             
      INCLUDE 'D0$LINKS:IZZTRH.LINK/LIST'                             
      INCLUDE 'D0$LINKS:IZZTRK.LINK/LIST'                             
      INCLUDE 'D0$LINKS:IZVTRH.LINK/LIST'                             
      INCLUDE 'D0$LINKS:IZDTRH.LINK/LIST'                             
      INCLUDE 'D0$LINKS:IZVTXT.LINK/LIST'                             
      INCLUDE 'D0$LINKS:IZDTRK.LINK/LIST'                             
      INTEGER NZ,STAT,IBIT,JBIT,LOCC,LOCV 
      INTEGER LVTXT,GZVTXT,LDTRK,GZDTRK,LZTRH,GZZTRH,LZTRK
      INTEGER NV,IV,NC,IC,NZTRAK,NRPHI 
      INTEGER IER,GOODC(NVMAX),GOODV(NVMAX),GOODVC(NVMAX)
      REAL PHIC,EPHIC,THETAC,PHIV(NVMAX),EPHIV,THETAV,XGC,YGC,XGV,YGV
      REAL ETHETC,ETHETV,QUALIT,TRD,IONIZ,PT,ERRTHE,DELTHE(NVMAX),DIFTHE
      LOGICAL EZERROR
      LOGICAL FIRST, MATCH(NVMAX)
      SAVE FIRST
      DATA PT/1./
      DATA FIRST/.TRUE./
C------------------------------------------------------------------------
C
      IF (FIRST) THEN
        CALL EZPICK('ZTRAKS_RCP')
        IF ( EZERROR(IER) ) THEN
          CALL ERRMSG('ZTRAKS','CVTRAK',
     &    'Unable to find bank ZTRAKS_RCP','W')
          GOTO 1000
        ENDIF
        CALL EZGET('ERRTHE',ERRTHE,IER)
        CALL EZGET('DIFTHE',DIFTHE,IER)
        CALL EZRSET
        FIRST=.FALSE.
      END IF
      NZ=0
      LZTRH=GZZTRH()
      IF (LZTRH.EQ.0) GO TO 1000
      LDTRH=LQ(LZTRH-IZDTRH)
      IF (LDTRH.EQ.0) GO TO 1000
      NC=IQ(LDTRH+2)
      IF (NC.LE.0) GO TO 1000
      LVTRH=LQ(LZTRH-IZVTRH)
      IF (LVTRH.EQ.0) GO TO 1000
      LVTXT=LQ(LVTRH-IZVTXT)
      LDTRK=LQ(LDTRH-IZDTRK)
      NV=IQ(LVTRH+2)
      NZTRAK=IQ(LZTRH+2)      ! Number of CD tracks from calls to ZTRAKS
      DO 200 IC=1,NC
        LOCC=GZDTRK(IC) 
        STAT=IQ(LOCC)
        IF (JBIT(STAT,IUSED).EQ.1) GO TO 200
        PHIC=Q(LOCC+6)
        THETAC=Q(LOCC+9)
        EPHIC=Q(LOCC+16)
        ETHETC=Q(LOCC+18)
        XGC=Q(LOCC+7)
        YGC=Q(LOCC+8)
        NRPHI=0
        DO 300 IV=1,NV
          LOCV=GZVTXT(IV) 
          STAT=IQ(LOCV)
          MATCH(IV)=0
          IF (JBIT(STAT,IUSED).EQ.1) GO TO 300
          PHIV(IV)=Q(LOCV+6)
          EPHIV=Q(LOCV+16)
          XGV=Q(LOCV+7)
          YGV=Q(LOCV+8)
          CALL CVRPHI(PHIV(IV),PHIC,PT,XGC,YGC,XGV,YGV,MATCH(IV))
          IF (MATCH(IV)) THEN   
            NRPHI=NRPHI+1
            GOODVC(IV)=0
            GOODC(IV)=0
            GOODV(IV)=0
            ETHETV=Q(LOCV+18)
            IF (ETHETC.GT.ERRTHE.AND.ETHETV.GT.ERRTHE) THEN
              MATCH(IV)=.FALSE.
            ELSE IF (ETHETC.LT.ERRTHE.AND.ETHETV.LT.ERRTHE) THEN
              THETAV=Q(LOCV+9)
              DELTHE(IV)=ABS(THETAC-THETAV)
              MATCH(IV)=DELTHE(IV).LT.DIFTHE 
              IF (MATCH(IV)) GOODVC(IV)=1
            ELSE IF (ETHETC.LT.ERRTHE.AND.ETHETV.GT.ERRTHE) THEN
              MATCH(IV)=.TRUE.
              GOODC(IV)=1
              GOODV(IV)=0
            ELSE IF (ETHETV.LT.ERRTHE.AND.ETHETC.GT.ERRTHE) THEN
              MATCH(IV)=.TRUE.
              GOODC(IV)=0
              GOODV(IV)=1
            END IF
          END IF
  300   CONTINUE
        DO 400 IV=1,NV
          IF (.NOT.MATCH(IV)) GO TO 400
          IF (NRPHI.GT.1.AND.GOODVC(IV).EQ.0) GO TO 400
          QUALIT = ABS(PHIV(IV)-PHIC)   ! Quality of track (temp. def.)
C            QUALIT=QUALIT/SQRT(EPHIC**2+EPHIV**2)
          CALL BKZTRK(LZTRK)
          IF (NRPHI.EQ.1) IQ(LZTRK)=IBSET(IQ(LZTRK),12)
          IF (GOODVC(IV).EQ.1) IQ(LZTRK)=IBSET(IQ(LZTRK),11)
          IF (GOODC(IV).EQ.1) IQ(LZTRK)=IBSET(IQ(LZTRK),10)
          IF (GOODV(IV).EQ.1) IQ(LZTRK)=IBSET(IQ(LZTRK),9)
          LOCC=GZDTRK(IC) 
          LOCV=GZVTXT(IV) 
          CALL MZFLAG(0,LOCC,IUSED,' ')
          CALL MZFLAG(0,LOCV,IUSED,' ')
          LQ(LZTRK-7)=LOCC
          LQ(LZTRK-6)=LOCV
          LZTRH=GZZTRH()
          IQ(LZTRH+2)=IQ(LZTRH+2)+1
          NZ=NZ+1
          IQ(LZTRK-5)=NZ+NZTRAK 
          IQ(LZTRK+2)=IV
          IQ(LZTRK+3)=IC
          Q(LZTRK+6)=QUALIT 
          IF (GOODVC(IV).EQ.1) THEN
            Q(LZTRK+8)=DELTHE(IV)
          ELSE
            Q(LZTRK+8)=99. 
          END IF
  400   CONTINUE
  200 CONTINUE
 1000 RETURN
      END
