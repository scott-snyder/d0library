      FUNCTION TRDPAR()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : run initialization for TRD package
C-
C-   Inputs  :
C-   Outputs : .TRUE. if run is to be processed (.FALSE. if skipped)
C-   Controls:
C-
C-   Created  13-FEB-1989   A. Zylberstejn
C-      Copied on VTRPAR (by D. Zieminska)
C-   Updated  23-OCT-1989   A. Zylberstejn  Remove HBOOK initialization
C-                                              (done in TRDBOK)
C-   Updated  12-OCT-1988   Daria Zieminska (RCP files)
C-   Updated  25-OCT-1989   J.Fr. Glicenstein  (mixing of 2 previous versions)
C-   Updated   2-FEB-1991   JFG   Suppress the call to the run STP file.
C-   Updated   4-MAY-1991   JFG Correct bug in call to DBL3
C-                     Resets HBOOK directories (modification by C.Stewart)
C-   Updated  13-AUG-1991   Susan K. Blessing   Change call to TDBINI to
C-    use logicals for pedestal and gain initialization.  This is faster,
C-    much more straightforward, and is how the other CD subdetectors do it.
C-   Updated  15-SEP-1991   A. Zylberstejn : Incorporate corrections to define
C-                                           COSMIC1
C-   Updated  29-DEC-1992   A. Zylberstejn
C-   Updated  17-JUL-1995   Lewis Taylor Goss  add DO_URANIUM flag
C-   Updated   9-DEC-1995   A. Zylberstejn  : test on cdd4 and cad1 for raw data
C-----------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL TRDPAR
      INCLUDE 'D0$INC:TCNTRL.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCDD4.LINK'
      INTEGER I,ICH,IW,NHB,TRCHAN,TRADDR,USUNIT,LENGTH
      INTEGER RECOVERSION,PASS,GZCAD1
      INTEGER LOUT,TRUNIT,DBM_FIRST_RUN,GZTHIT
      LOGICAL BYPASS_DBL3_ERROR,OK, FIRST,RESET,DB_OPENED,DO_URANIUM
      CHARACTER*(*) GEOFIL
      CHARACTER*(*) CLBDAT
      CHARACTER*26 FILNAM, DFLSTP
      CHARACTER*4 SELCLB
      CHARACTER*(*) HBKDIR
      PARAMETER (HBKDIR='HBOOK_DIRECTORY')
      PARAMETER (GEOFIL='TRD_STPFILE')
      PARAMETER (CLBDAT='CALIB_DATA')
      INTEGER RUN,RUNNO,IERR,IDX,IER,MAX_TRDCRT,CALTYPE
C      real hva(3,16),HVP(3,16),HVW(3,16),GAIN,SLOPE
C      INTEGER TIME,DATE
      LOGICAL PD_INI,GN_INI,READ_DBMON
C
      PARAMETER (IDX=1)
      DATA FIRST/.TRUE./
      DATA PD_INI,GN_INI/2*.FALSE./
      DATA DB_OPENED /.FALSE./
C
C------------------------------------------------------------------------------
      TRDPAR = .TRUE.
      IF (FIRST) THEN
        FIRST=.FALSE.
        LOUT=TRUNIT()
        CALL EZPICK('TRD_RCP')
        CALL EZGET('TEXAMIN',TEXAMIN,IER)
        CALL EZGET('TTRAKIN',TTRAKIN,IER)
        READ_DBMON=.FALSE.
        CALL EZGET('READ_DBMON',I, IER)
        IF(IER.EQ.0 .AND. I.NE.0)READ_DBMON=.TRUE.
        DBM_FIRST_RUN=100000
        CALL EZGET('DBM_FIRST_RUN', I, IER)
        IF(IER.EQ.0)DBM_FIRST_RUN=I
        CALL EZGETS(GEOFIL,IDX,DFLSTP,LENGTH,IER)
        CALL EZGETS(CLBDAT,IDX,SELCLB,LENGTH,IER)
C Get number of crates, count from zero
        CALL EZGET('MAX_TRDCRT',MAX_TRDCRT,IER)
        IF (IER.NE.0) THEN
          CALL INTMSG(' MAX_TRDCRT NOT SET IN TRD_RCP.')
          MAX_TRDCRT = 0
        END IF
C ****  check if reset histograms upon begin run
C
        CALL EZGET('HRESET',RESET,IER)
        CALL EZGET('BYPASS_DBL3_ERROR',BYPASS_DBL3_ERROR,IER)
        IF(IER.NE.0)BYPASS_DBL3_ERROR=.FALSE.
        CALL EZRSET
C ***   constants definition
        CALL TRDDEF
        MCDATA=.FALSE.
        COSMIC1=.FALSE.
        IF (IQ(LHEAD+1) .GT. 1000) MCDATA = .TRUE.
        IF(.NOT.MCDATA .AND. IQ(LHEAD+6).LT.22158)COSMIC1=.TRUE.
C      IF (MCDATA) THEN
C      Reads the default STP to get the geometry
        FILNAM=DFLSTP
C      ELSE
C        RUN=RUNNO()
C        WRITE (FILNAM,100) RUN
C  100   FORMAT('D0$TRD_GEO:STP_',I7.7,'.ZEB')
C      ENDIF
C ***
        CALL INTMSG(' READ STATIC PARAMETER FILE '//FILNAM)
        CALL TRDCON(FILNAM, IERR)
        IF (IERR.NE.0) THEN
          CALL INTMSG(' TRDPAR: CAN NOT OPEN FILE ',FILNAM)
          TRDPAR = .FALSE.
          GOTO 999
        ELSE
C ***** Reads in TRD_RCP.
          CALL TRREAD
        END IF
        CALTYPE = 0
        IF (TTRAKIN) THEN
          CALL TRDBOK
        ENDIF
        IF (TEXAMIN) THEN
          CALL TRDBOK_ON
        ENDIF
      END IF
C
C ****  reset histograms upon begin run if reset is true
C
      IF(RESET) THEN
        IF (TTRAKIN) THEN
          CALL DHDIR('TRD_RCP',HBKDIR,IER,' ')
          CALL HRESET(0,' ')
        ENDIF
        IF (TEXAMIN) THEN
          CALL DHDIR('TRDHIT_RCP',HBKDIR,IER,' ')
          CALL HRESET(0,' ')
        ENDIF
      END IF
C
C  Read in TRD STP values from DBL3, if necessary
C  Overwrites pedestals and gains from default file.
      CALL RECO_VERSION(RECOVERSION,PASS)
      RUN=RUNNO()
      IF (.NOT.MCDATA .AND. (GZTHIT().EQ.0 .OR.RECOVERSION.LT.12)) THEN
        IF(LQ(LHEAD-IZCDD4).EQ.0 .AND. GZCAD1().LE.0)THEN
          CALL ERRMSG('THIT corrupted,no raw data  ','TRDPAR',
     &                'cannot get constants for TRD ','W')
          GO TO 999
        END IF
C Protection in case no event has come yet.
        IF (RUN.LE.0) THEN              ! Takes the most recent values
                                        ! in DBL3: provisionnal fix
          RUN = 999999
        ENDIF
        IF (SELCLB.EQ.'ALL ') THEN
          PD_INI = .TRUE.
          GN_INI = .TRUE.
        ELSE IF (SELCLB.EQ.'PEDS') THEN
          PD_INI = .TRUE.
        ELSE IF (SELCLB.EQ.'GAIN') THEN
          GN_INI = .TRUE.
        ENDIF
C open all databases
        CALL TDBINI(RUN,MAX_TRDCRT,PD_INI,GN_INI,DB_OPENED,OK)
        IF (.NOT.OK) THEN
          IF(BYPASS_DBL3_ERROR)THEN
            CALL INTMSG(' TRDPAR: ERROR IN TDBINI.')
            GO TO 999
          ELSE
            TRDPAR = .FALSE.
            CALL ERRMSG('TTRAKS','TRDPAR',' ERROR IN DBL3','F')
            GO TO 999
          END IF
        END IF
        DO_URANIUM = .TRUE.
        IF(READ_DBMON .AND. RUN.GE.DBM_FIRST_RUN)
     &    CALL TREAD_MONITOR(DB_OPENED,DO_URANIUM)
C close all databases
        CALL TDBINI(RUN,MAX_TRDCRT,.FALSE.,.FALSE.,DB_OPENED,OK)
      END IF
  999 CONTINUE
      CALL TRD_NWIRE_PER_LAYER
      RETURN
      END
