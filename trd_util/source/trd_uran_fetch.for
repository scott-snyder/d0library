      SUBROUTINE TRD_URAN_FETCH(RUN_NO,TIME_OF_RUNU,CLASS,
     &  NDEV_URAN,VAL_URAN,DB_OPENED,IOK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Extract the Processed STP structures from DBL3 into
C-                         memory
C-
C-   Inputs  : RUN_NO = Run number
C-   Input/Output : DB_OPENED: indicates if database has been opened yet
C-   Controls: returns IOK = .TRUE. if all goes well
C-
C-   Created  20-JAN-1995   Lewis Taylor Goss
C-   Updated  17-JUL-1995   Lewis Taylor Goss -- make infinity 999999.9
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBSTP.INC'
C
      INTEGER RUN_NO,LENGTH,IER,LTURA,LKEY,LORUN,HIRUN,IDAT,ITIM,M,K
      INTEGER FORCE_RUN_NUMBER
      PARAMETER(M=200)
      INTEGER DBL3_TIME,I,NDEV_URAN,URAN_RUN_STOR(M),LEN,N,N_ZONES
      INTEGER IRUN,ILAY,TIME_OF_RUNU(2),KEYS(19,2),J,CRATE
C
      REAL VAL_URAN(14),QJT_UR,UR_STOR(3,M),QJT_UR_STOR(M),PRES_STOR(M)
      REAL TEMP_STOR(M),HV_STOR(3,M),HP_STOR(3,M)
C
      CHARACTER*25 PATH
      CHARACTER*7 URANIUM_DB
      CHARACTER*80 DBCALIB
      CHARACTER*11 CLASS
C
      LOGICAL IOK,DB_OPENED,FIRST,BYPASS_DBL3_ERROR,FORCE_RUN,RUN1A
      CHARACTER*1 SEVER
C
      SAVE FIRST, LORUN, HIRUN
C
      DATA FIRST /.TRUE./
      DATA CRATE / 0 /
      DATA LORUN / 999999999 /
      DATA HIRUN / -1 /
      COMMON /URLINK/ LTURA
C----------------------------------------------------------------------
C
C  init...
C
      IOK = .TRUE.
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('TRD_RCP')
        CALL EZGETS('DBCALIB$TRD',1,DBCALIB,LENGTH,IER)
        CALL EZGET('BYPASS_DBL3_ERROR',BYPASS_DBL3_ERROR,IER)
        CALL EZGET('FORCE_RUN',FORCE_RUN,IER)
        CALL EZGET('FORCE_RUN_NUMBER',FORCE_RUN_NUMBER,IER)
        CALL EZGETS('URANIUM_DB',1,URANIUM_DB,LEN,IER)
        URANIUM_DB='CALIB'
        IF (URANIUM_DB.EQ.'TRD.RCP') THEN     ! READ FROM RCP FILE
          CALL EZGETA ('URANIUM',0,0,0,N,IER)
          N_ZONES=N/14
          DO I=1,N_ZONES
            K=(I-1)*14
            CALL EZGETA ('URANIUM',K+1,K+1,1,URAN_RUN_STOR(I),IER)
            CALL EZGETA ('URANIUM',K+2,K+4,1,UR_STOR(1,I),IER)
            CALL EZGETA ('URANIUM',K+5,K+5,1,PRES_STOR(I),IER)
            CALL EZGETA ('URANIUM',K+6,K+6,1,TEMP_STOR(I),IER)
            CALL EZGETA ('URANIUM',K+7,K+7,1,IDAT,IER)
            CALL EZGETA ('URANIUM',K+8,K+8,1,ITIM,IER)
            CALL EZGETA ('URANIUM',K+9,K+11,1,HV_STOR(1,I),IER)
            CALL EZGETA ('URANIUM',K+12,K+14,1,HP_STOR(1,I),IER)
            CALL CLDR(92,IDAT,ITIM,QJT_UR_STOR(I))
          ENDDO
        ENDIF
C
        CALL EZRSET
        SEVER = 'F'
        IF (BYPASS_DBL3_ERROR) SEVER = 'W'
      ENDIF
C
C  Check top level STP bank
C
      IF (LTGEN.LE.0) THEN
        CALL ERRMSG('Uranium dat ','TRD_URAN_FETCH',
     &    'TRD constants header bank does not exist',SEVER)
        IOK = .FALSE.
        GO TO 999
      ENDIF
C
      IF (RUN1A()) THEN
        DO I = 1,14
          VAL_URAN(I) = 999.
        ENDDO
      ELSE
        IF (URANIUM_DB.EQ.'TRD.RCP') THEN
          DO I = 1,N_ZONES - 1
            IF (RUN_NO.LE.URAN_RUN_STOR(I).AND.I.EQ.1) THEN
              IRUN = 1
            ELSEIF(RUN_NO.GT.URAN_RUN_STOR(I).AND.RUN_NO.LE.
     &        URAN_RUN_STOR(I+1)) THEN
              IRUN = I
            ELSEIF(I.EQ.N_ZONES-1.AND.RUN_NO.GT.URAN_RUN_STOR(I+1)) THEN
              IRUN = N_ZONES
            ENDIF
          ENDDO
          VAL_URAN(1) = URAN_RUN_STOR(IRUN)
          IF (IRUN.LE.N_ZONES-1) THEN
            VAL_URAN(2) = URAN_RUN_STOR(IRUN+1) - 1
          ELSE
            VAL_URAN(2) = 999999.9
          ENDIF
          VAL_URAN(3) = QJT_UR_STOR(IRUN)
          VAL_URAN(4) = PRES_STOR(IRUN)
          VAL_URAN(5) = TEMP_STOR(IRUN)
          DO ILAY = 1,3
            VAL_URAN( 5+ILAY) = UR_STOR(ILAY,IRUN)
            VAL_URAN( 8+ILAY) = HV_STOR(ILAY,IRUN)
            VAL_URAN(11+ILAY) = HP_STOR(ILAY,IRUN)
          ENDDO
        ELSEIF (URANIUM_DB(1:5).EQ.'CALIB') THEN
C  Open DBL3 database if not already opened (only if necessary!)
          IF ( RUN_NO .GE. LORUN .AND.
     &         RUN_NO .LE. HIRUN ) GO TO 999 ! No need to open DB
          IF (.NOT.DB_OPENED) THEN
            IF (FORCE_RUN) THEN
              CALL DBCLB_INITIALIZE_FORCE_RUN(FORCE_RUN_NUMBER)
            ENDIF
            CALL DBCLB_INITIALIZE(DBCALIB,'S',IOK)
            IF (.NOT.IOK) THEN
              CALL ERRMSG('TRD uranium','TRD_URAN_FETCH',
     &          'Error initializing DBL3 database',SEVER)
              GO TO 999
            ELSE
              DB_OPENED = .TRUE.
            ENDIF
          ENDIF
C
          CALL INTMSG('Reading TRD Uran. Noise from DBL3-offline path')
          CALL DBCLB_PATH(CLASS,'TRD',PATH)
          CALL DBCLB_FETCH_OFFLINE(PATH,RUN_NO,CRATE,LTURA,LKEY)
          IF (LTURA .EQ. 0 .OR. LKEY .EQ. 0) THEN
            CALL ERRMSG('TRD offline data base','TRD_URAN_FETCH',
     &        'Error fetching uranium info ',SEVER)
            IOK = .FALSE.
            GO TO 999
          ENDIF
          LORUN = IC(LKEY+3)
          HIRUN = IC(LKEY+4)
          VAL_URAN(1) = IC(LKEY+11)                 ! Uranium run number
          IF(IC(LKEY+ 4).NE.999999999) THEN
            VAL_URAN(2) = IC(LKEY+ 4)! Upper limit
          ELSE
            VAL_URAN(2) = 999999.9
          ENDIF
          DBL3_TIME   = IC(LKEY+12)
          CALL D3UUT(TIME_OF_RUNU,DBL3_TIME)
          CALL DBUPTS(IDAT,ITIM,DBL3_TIME)
          CALL CLDR(92,IDAT,ITIM,QJT_UR)
          VAL_URAN(3) = QJT_UR
          DO I = 4,NDEV_URAN
            VAL_URAN(I) = C(LTURA+I-2)
          ENDDO
        ELSEIF (URANIUM_DB(1:5).EQ.'CDFIX') THEN
          CALL D3UPT(TIME_OF_RUNU,KEYS(3,1))
          KEYS(4,1) = KEYS(3,1)
          CALL FETCH_URAN_CDFIX(KEYS)
C
          VAL_URAN(1) = KEYS(8,1)
          IF (KEYS(4,1).NE.999999999) THEN
            VAL_URAN(2) = KEYS(8,2) - 1
          ELSE
            VAL_URAN(2) = 999999.9
          ENDIF
          CALL D0DBL3_DBUPTS(IDAT,ITIM,KEYS(3,1))
          CALL CLDR(92,IDAT,ITIM,VAL_URAN(3))
          CALL D3UUT(TIME_OF_RUNU,KEYS(3,1))
          VAL_URAN(4) = FLOAT(KEYS(12,1))/10.
          VAL_URAN(5) = FLOAT(KEYS(13,1))/10.
          DO J = 1,3
            VAL_URAN( 5+J) = FLOAT(KEYS( 8+J,1))/10.
            VAL_URAN( 8+J) = FLOAT(KEYS(13+J,1))/10.
            VAL_URAN(11+J) = FLOAT(KEYS(16+J,1))/10.
          ENDDO
        ENDIF
      ENDIF
C
  999 RETURN
      END
