      FUNCTION GLOBTZ_EVT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : get drift time distributions from VWDA
C-                         bank for GLOBAL TZERO online calculation
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  28-JAN-1993             V. D. Elvira
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      LOGICAL GLOBTZ_EVT
      LOGICAL FIRST
      LOGICAL OK
C----------------------------------------------------------------------
      REAL TMIN,RAW_TIME,OLDTZ(0:2,0:31,0:7,0:1),OLDSG(0:2,0:31,0:7,0:1)
      REAL IMI,IMA
C----------------------------------------------------------------------
      INTEGER LAYER,SECTOR,WIRE,IEND,LVWDA,GZVWDA,NEL,NWORDS,POINT,
     &  POINTER,LVTMW,GZVTMW,RUNNUM,RUNNO,PREV_RUN
      INTEGER NLRS,NSCT(3),NWRE,LRS(3),SCTS0(16),SCTS1(32),WRE(8)
      INTEGER IL,IS,IW,HIT,NPUL,ID,IER,IERR
      INTEGER CHA,CRATE,CR
      INTEGER I,NX,NUM,LRCP
C----------------------------------------------------------------------
      CHARACTER*40 CRTIT0
      CHARACTER*20 RCPFIL
      CHARACTER*3 CHCRA
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C----------------------------------------------------------------------
      DATA LRS/0,1,2/
      DATA SCTS0/0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15/
      DATA SCTS1/0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,
     &  21,22,23,24,25,26,27,28,29,30,31/
      DATA WRE/0,1,2,3,4,5,6,7/
      DATA FIRST/.TRUE./
      DATA PREV_RUN/-1/
C----------------------------------------------------------------------
      GLOBTZ_EVT=.TRUE.
C----------------------------------------------------------------------
      NLRS=3
      NSCT(1)=16
      NSCT(2)=32
      NSCT(3)=32
      NWRE=8
C----------------------------------------------------------------------
      IF (FIRST) THEN
	FIRST=.FALSE.
        DO IL=1,NLRS
          LAYER=LRS(IL)
          LVTMW=GZVTMW(LAYER)
          DO IS=1,NSCT(IL)
            IF (LAYER.EQ.0) THEN
              SECTOR=SCTS0(IS)
            ELSE
              SECTOR=SCTS1(IS)
            ENDIF
            DO IW=1,NWRE
              WIRE=WRE(IW)
              POINTER=LVTMW+(SECTOR*IC(LVTMW+4)+WIRE)*5+5
              OLDTZ(LAYER,SECTOR,WIRE,0)=C(POINTER+1)
              OLDSG(LAYER,SECTOR,WIRE,0)=C(POINTER+2)
              OLDTZ(LAYER,SECTOR,WIRE,1)=C(POINTER+3)
              OLDSG(LAYER,SECTOR,WIRE,1)=C(POINTER+4)
            ENDDO
          ENDDO
        ENDDO
      ENDIF
C----------------------------------------------------------------------
      RUNNUM=RUNNO()
C----------------------------------------------------------------------
      IF (PREV_RUN.NE.RUNNUM) THEN
        PREV_RUN=RUNNUM
        RCPFIL = 'GLOBTZ_RCP'
        CALL EZLOC(RCPFIL,LRCP)
        IF ( LRCP .LE. 0 ) THEN
          CALL INRCP (RCPFIL,IER)  ! Read parameter file into an SRCP bank
          IF ( IER .NE. 0 ) THEN
            CALL ERRMSG('INRCP ERROR','GLOBTZ_INI',
     &      'Attempt to read GLOBTZ_RCP failed','F')
          ENDIF
        ENDIF
        CALL EZPICK('GLOBTZ_RCP')
        CALL EZGET('NX',NX,IER)
        CALL EZGET('IMI',IMI,IER)
        CALL EZGET('IMA',IMA,IER)
        CALL EZRSET
C----------------------------------------------------------------------
C- Book TZERO crate histograms
C----------------------------------------------------------------------
        CALL DHDIR(' ','//PAWC/VTXTZ',IERR,' ')
        IF (IERR.NE.0) THEN
          CALL ERRMSG('VTRAKS','VTX_EXM_TZ',
     &      ' ERROR SETTING HBOOK DIRECTORY://PAWC/VTXTZ','W')
        ENDIF
        DO CR=1,10
          NUM=CR*10-7
          WRITE(CHCRA,100) NUM
          CRTIT0='GLOBAL TZERO for crate '//CHCRA
          CALL HBOOK1(100+10*(CR-1),CRTIT0,NX,IMI,IMA,0.)
          CALL HBARX(100+10*(CR-1))
        ENDDO
  100   FORMAT(I2)
      ENDIF
C----------------------------------------------------------------------
C-  Loop over all channels geting the earliest hit and filling drift
C-  time distributions
C----------------------------------------------------------------------
      CALL DHDIR(' ','//PAWC/VTXTZ',IERR,' ')
      IF (IERR.NE.0) THEN
        CALL ERRMSG('VTRAKS','VTX_EXM_TZ',
     &    ' ERROR SETTING HBOOK DIRECTORY://PAWC/VTXTZ','W')
      ENDIF
C----------------------------------------------------------------------
      DO 1 IL=1,NLRS
        LAYER=LRS(IL)
        DO 2 IS=1,NSCT(IL)
          IF (LAYER.EQ.0) THEN
            SECTOR=SCTS0(IS)
          ELSE
            SECTOR=SCTS1(IS)
          ENDIF
          LVWDA=GZVWDA(LAYER,SECTOR)
          IF (LVWDA.EQ.0) GOTO 2
          NEL=IQ(LVWDA+2)
          NWORDS=IQ(LVWDA+3)
          DO 3 IW=1,NWRE
            WIRE=WRE(IW)
            DO 4 IEND=0,1
              NPUL=IQ(LVWDA+2*WIRE+4+IEND)
              IF (NPUL.EQ.0) GOTO 4
              POINT=IQ(LVWDA+NEL+2*WIRE+4+IEND)
              TMIN=99999.
              DO 5 HIT=1,NPUL
                RAW_TIME = Q(LVWDA+POINT+(HIT-1)*NWORDS+1)
                TMIN=AMIN1(TMIN,RAW_TIME)
    5         CONTINUE
              ID=10000+512*LAYER+16*SECTOR+2*WIRE+IEND
              CALL VTX_TRANS_CHAN(ID-10000,CRATE,CHA,1)
              CR=INT(FLOAT(CRATE+7)/10.)
              IF (OLDSG(LAYER,SECTOR,WIRE,IEND).NE.0.) THEN 
                CALL HF1(100+10*(CR-1),TMIN-OLDTZ(LAYER,SECTOR,WIRE,
     &            IEND),1.)
              ENDIF
    4       CONTINUE
    3     CONTINUE
    2   CONTINUE
    1 CONTINUE
  999 RETURN
      END
