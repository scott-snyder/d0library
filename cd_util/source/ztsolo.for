      SUBROUTINE ZTSOLO 
C------------------------------------------------------------------------
C 
C  Make Central Detector track banks (ZTRK) for unmatched CDC,FDC and VTX
C  tracks. Increment number of CD tracks in bank ZTRH (header for CD tracks).
C
C  Daria Zieminska Jan 1990
C  
C------------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'                             
      INCLUDE 'D0$LINKS:IZZTRH.LINK/LIST'                             
      INCLUDE 'D0$PARAMS:ZTRAKS.PARAMS'                             
      INCLUDE 'D0$LINKS:IZZTRK.LINK/LIST'                             
      INCLUDE 'D0$LINKS:IZVTRH.LINK/LIST'                             
      INCLUDE 'D0$LINKS:IZFTRH.LINK/LIST'                             
      INCLUDE 'D0$LINKS:IZDTRH.LINK/LIST'                             
      INCLUDE 'D0$LINKS:IZVTXT.LINK/LIST'                             
      INCLUDE 'D0$LINKS:IZFDCT.LINK/LIST'                             
      INCLUDE 'D0$LINKS:IZDTRK.LINK/LIST'                             
      INTEGER NZ,STAT,IBIT,JBIT,LOCC,LOCV,LOCF 
      INTEGER LVTXT,GZVTXT,LDTRK,GZDTRK,LFDCT,GZFDCT
      INTEGER  LDTRH,LVTRH,LFTRH
      INTEGER LZFIND,LZTRH,GZZTRH,LZTRK 
      INTEGER NV,IV,NC,IC,NF,IF,NZTRAK 
      INTEGER IXZTRK,MBOOKT(5),IBOOKT,IER
      REAL IONIZ 
      LOGICAL FIRST,VTSOLO 
      DATA FIRST/.TRUE./
      IF (FIRST) THEN
C
        CALL EZPICK('ZTRAKS_RCP')
        CALL EZGET('VTSOLO',VTSOLO,IER)
        FIRST=.FALSE.
      END IF
      NZ=0
      NC=0
      NV=0
      NF=0
      NZTRAK=0
      LVTRH=0
      LDTRH=0
      LFTRH=0
      LZTRH=GZZTRH()
      IF(LZTRH.NE.0) THEN
        LVTRH=LQ(LZTRH-IZVTRH)
        LDTRH=LQ(LZTRH-IZDTRH)
        LFTRH=LQ(LZTRH-IZFTRH)
        NZTRAK=IQ(LZTRH+2) ! No. of CD tracks from calls to ZTRAKS
      ELSE
        GOTO 1000
      ENDIF
      IF(LVTRH.NE.0) THEN
        LVTXT=LQ(LVTRH-IZVTXT)
        NV=IQ(LVTRH+2)
      ENDIF
      IF(LDTRH.NE.0) THEN 
        LDTRK=LQ(LDTRH-IZDTRK)
        NC=IQ(LDTRH+2)
      ENDIF
      IF(LFTRH.NE.0) THEN 
        LFDCT=LQ(LFTRH-IZFDCT)
        NF=IQ(LFTRH+2)
      ENDIF
      IF (NC.EQ.0) GO TO 201
      DO 200 IC=1,NC
        LOCC=GZDTRK(IC) 
        STAT=IQ(LOCC)
        IF (JBIT(STAT,IUSED).EQ.1) GO TO 200
        CALL MZFLAG(0,LOCC,IUSED,' ')
        NZ=NZ+1
        LZTRH=GZZTRH()
        IQ(LZTRH+2)=IQ(LZTRH+2)+1
        CALL BKZTRK(LZTRK)
        LOCC=GZDTRK(IC) 
        LQ(LZTRK-7)=LOCC
        IQ(LZTRK-5)=NZ+NZTRAK 
        IQ(LZTRK+2)=0
        IQ(LZTRK+3)=IC
        IQ(LZTRK+4)=0 
        Q(LZTRK+5)=0.
        Q(LZTRK+6)=0. 
        Q(LZTRK+7)=0.
  200 CONTINUE
  201 IF (NV.EQ.0.OR.VTSOLO.EQV..FALSE.) GO TO 301
      DO 300 IV=1,NV
        LOCV=GZVTXT(IV) 
        STAT=IQ(LOCV)
        IF (JBIT(STAT,IUSED).EQ.1) GO TO 300
        CALL MZFLAG(0,LOCV,IUSED,' ')
        NZ=NZ+1
        LZTRH=GZZTRH()
        IQ(LZTRH+2)=IQ(LZTRH+2)+1
        CALL BKZTRK(LZTRK)
        LOCV=GZVTXT(IV) 
        LQ(LZTRK-6)=LOCV
        IQ(LZTRK-5)=NZ+NZTRAK 
        IQ(LZTRK+2)=IV
        IQ(LZTRK+3)=0
        IQ(LZTRK+4)=0 
        Q(LZTRK+5)=0.
        Q(LZTRK+6)=0. 
        Q(LZTRK+7)=0.
  300 CONTINUE
  301 IF (NF.EQ.0) GO TO 1000
      DO 400 IF=1,NF
        LOCF=GZFDCT(IF) 
        STAT=IQ(LOCF)
        IF (JBIT(STAT,IUSED).EQ.1) GO TO 400
        CALL MZFLAG(0,LOCF,IUSED,' ')
        NZ=NZ+1
        LZTRH=GZZTRH()
        IQ(LZTRH+2)=IQ(LZTRH+2)+1
        CALL BKZTRK(LZTRK)
        LOCF=GZFDCT(IF) 
        LQ(LZTRK-8)=LOCF
        IQ(LZTRK-5)=NZ+NZTRAK 
        IQ(LZTRK+2)=0
        IQ(LZTRK+3)=0
        IQ(LZTRK+4)=IF 
        IQ(LZTRK+5)=0
        Q(LZTRK+6)=0. 
        Q(LZTRK+7)=0.
        Q(LZTRK+8)=0.
  400 CONTINUE
 1000 RETURN
      END
