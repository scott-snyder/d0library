      SUBROUTINE FDC_LUMADJ(RUN,GAIN_ADJ,STATUS,CHANGED)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Compute gain adjustment factor in the FDC
C-           correlated with Luminosity.
C-           Use luminosity from special LUMINOSITY.DAT file for runs before
C-           'LUM_START_DBM' (=57606).
C-           Use luminosity estimated from 'VTX_HV.LUM' in DBMON$GLB for
C-           later runs.
C-           Note that a different parameterization is needed during the
C-           period when the gains in the FDC were affected by Alcohol in the 
C-           gas.
C-               
C-   Inputs  : LUMINOSITY.DAT -- ASCII file containg time-stamp and Luminosity
C-             or DBMON$GLB for later runs.
C-   Outputs : GAIN_ADJ  = gain adjustment factors FDC half and type.
C-             STATUS = 0 --> OK
C-                      1 --> Luminosity data is stale
C-                     -1 --> Luminosity correction not available
C-             CHANGED     = .TRUE. if calibration has changed, else .FALSE.
C-
C-   Created   6-APR-1993   Robert E. Avery  Based on VTX_LUMADJ by Ed Oltman
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:DBMUKEY.INC'
c I/O:
      INTEGER RUN
      INTEGER STATUS
      LOGICAL CHANGED
      REAL    GAIN_ADJ(0:1,0:1)
C Locals:
      INTEGER EARLY_TIME
      PARAMETER( EARLY_TIME = 478380083 ) !4-MAR-92 12:08:11
      INTEGER ERR,ERR1,TIME_1,LAST_TIME,ERROR
      INTEGER START_DBM
      INTEGER VAXTIME(2),I
      INTEGER DBL3_TIME,LAST_GOOD_TIME
      INTEGER LAST_RUN,LEN                         
      INTEGER HALF, UNIT 
      INTEGER ENTRY_LAST,ENTRY
C
      REAL    LUM_CORR,RATE
      REAL    XHOU,MAX_TIME,LUM,AGE 
      REAL    LUM_ADJ(0:1,0:1)
      CHARACTER*80 TXT
      CHARACTER*1  SEVER
      LOGICAL FIRST,NO_CORRECTION,LAST_FRESH,NEW
      LOGICAL BYPASS_DBL3_ERROR
      LOGICAL LVAR 
      LOGICAL ONE_GOOD 
      LOGICAL BACKSPACE 
      LOGICAL GOOD
c Externals:
      INTEGER TRULEN,RUNNO
      LOGICAL D3UPT
      LOGICAL D3UUT
      REAL    D3UXH
c Data:
      DATA FIRST/.TRUE./
      DATA NO_CORRECTION/.FALSE./
      DATA LAST_RUN/-1/
      DATA ENTRY_LAST/-1/
C----------------------------------------------------------------------
      BACKSPACE = .FALSE.
      STATUS = -1
      CHANGED = .FALSE.
      IF (NO_CORRECTION) GO TO 999
C
      IF (FIRST) THEN 
        FIRST = .FALSE.
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET('BYPASS_DBL3_ERROR',BYPASS_DBL3_ERROR,ERR)
        ERROR = ERR
        CALL EZGET('LUM_MAX_TIME',MAX_TIME,ERR)
        ERROR = ERROR + ERR
        CALL EZGET('LUM_START_DBM',START_DBM,ERR)
        ERROR = ERROR + ERR
        CALL EZGET('LUM_CONV',RATE,ERR)
        ERROR = ERROR + ERR
        CALL EZGET('LUM_CORR',LUM_CORR,ERR)
        ERROR = ERROR + ERR
        CALL EZRSET
        SEVER = 'F'
        IF (BYPASS_DBL3_ERROR) SEVER = 'W'
        IF (ERROR .NE. 0) THEN
          CALL ERRMSG('FDC LUM corr NOT made','FDC_LUMADJ',
     &      'FTRAKS_RCP errors -- cannot apply Lumin based corr.',SEVER)
          NO_CORRECTION = .TRUE.
        ENDIF
      ENDIF
      RUN = RUNNO() 
      IF (RUN .NE. LAST_RUN) THEN
        LAST_RUN = RUNNO()
        LAST_GOOD_TIME = EARLY_TIME
        LAST_TIME = 0
        ONE_GOOD = .FALSE.
        CALL FDC_GETLUMLIST(RUN,LUM_ADJ,ENTRY)
        IF ( ENTRY.NE.ENTRY_LAST ) THEN
          ENTRY_LAST=ENTRY
          IF ( ENTRY.EQ.0 ) GOTO 200
        ENDIF
      ENDIF
      IF ( ENTRY.EQ.0 ) GOTO 999
C
C ----------------->           NORMAL ENTRY
C
      VAXTIME(1) = IQ(LHEAD+4)
      VAXTIME(2) = IQ(LHEAD+5)
      LVAR = D3UPT(VAXTIME,DBL3_TIME)
C
 10   CONTINUE
      AGE = D3UXH(DBL3_TIME,EARLY_TIME)
      IF ( RUN.LT.START_DBM ) THEN
        CALL VTX_GETLUM(VAXTIME,DBL3_TIME,LUM,XHOU,TIME_1,ERR)
      ELSEIF (AGE.GT.0.0) THEN
        CALL DBMU_GETDM('DBM_VTX_HV_C','VTX_HV.LUM',1,VAXTIME,
     &    1,' ',LUM,XHOU,ERR)
C
        TIME_1 = DBKEYS(3) 
      ELSE
        ERR = -1
      ENDIF
      IF (ERR .LT. 0) THEN
        NO_CORRECTION = .TRUE.
        CALL ERRMSG('FDC LUM corr NOT made','LUM_LUMADJ',
     &    'Cannot get luminosity-- no LUM corrections made',
     &    SEVER)
        STATUS = -1
        LUM=0
        GOTO 200
      ENDIF
      STATUS = 1
c
      AGE = D3UXH(DBL3_TIME,LAST_GOOD_TIME)
      LAST_FRESH= AGE .LT. MAX_TIME
      NEW       = TIME_1 .NE. LAST_TIME
      LAST_TIME = TIME_1
C
      IF (.NOT. NEW) THEN
        IF ( .NOT. LAST_FRESH ) THEN
C
C ****  Should be a new record -- existing data is stale
C
          ERR1 = ERR1 + 1
          IF (ERR1 .EQ. 1) THEN
            WRITE(TXT,'(A,F7.2,A)') 
     &        'LUMINOSITY data is stale (',XHOU,'hours) using anyway'
            LEN = TRULEN(TXT)
            CALL ERRMSG('FDC LUM corr stale','FDC_LUMADJ',
     &        TXT(1:LEN), 'W')
          ENDIF
        ELSE
          STATUS = 0
        ENDIF
        GO TO 999
      ENDIF
C
C ****  This section for NEW record
C
      GOOD=.TRUE.
      IF ( RUN.GE.START_DBM ) THEN
        LUM = 1. - LUM_CORR*RATE*LUM
        IF ( (LUM .GT. 0.) .AND. (LUM .LT. 1.) ) THEN
          LUM = -ALOG(LUM)/LUM_CORR
        ELSE
          GOOD=.FALSE.
        ENDIF
      ENDIF
      IF (.NOT. ONE_GOOD) THEN
C
C ****      ONE_GOOD = .FALSE ==> no valid (e.g GOOD = .TRUE.) DBL3 record has
C ****      been read for this run.  Start looking backwards in database for 
C ****      valid data and use it until there is valid data.
C
        IF (GOOD) THEN
          ONE_GOOD = .TRUE.
          IF (BACKSPACE) THEN
            VAXTIME(1) = IQ(LHEAD+4)
            VAXTIME(2) = IQ(LHEAD+5)
            LVAR = D3UPT(VAXTIME,DBL3_TIME)
            XHOU = D3UXH(DBL3_TIME,DBKEYS(3))
          ENDIF
        ELSE
          BACKSPACE = .TRUE.
          CALL DBINCT(DBKEYS(3),-10,DBL3_TIME)
          LVAR = D3UUT(VAXTIME,DBL3_TIME)
          GO TO 10
        ENDIF
      ENDIF
      IF ( GOOD ) THEN
        LAST_GOOD_TIME = LAST_TIME
        LAST_FRESH = XHOU .LT. MAX_TIME
        AGE = XHOU
      ENDIF
      IF( LAST_FRESH ) THEN
        STATUS = 0
        IF (ERR1 .GT. 0) THEN
          TXT = 'D0LUM data is fresh again'
          LEN = TRULEN(TXT)
          CALL ERRMSG('FDC LUM corr OK','FDC_LUMADJ',TXT(1:LEN),'W')
          ERR1 = 0
        ENDIF
      ELSE
        STATUS = 1
        IF (ERR1 .EQ. 0) THEN
          WRITE(TXT,'(A,F7.2,A)') 
     &      'LUMINOSITY data is stale (',XHOU,'hours) using anyway'
          LEN = TRULEN(TXT)
          CALL ERRMSG('FDC LUM corr stale','FDC_LUMADJ',TXT(1:LEN),'W')
        ENDIF
        ERR1 = ERR1 + 1
      ENDIF
C
C ****  OK, NOW COMPUTE GAIN CORRECTION DUE TO 
C ****  "Mysterious unknown Luminosity effect".  
C ****  LUM is the luminosity corrected for multiple interactions.
C
  200 CONTINUE
      DO UNIT =  0, 1
        DO HALF =  0, 1
          GAIN_ADJ(HALF,UNIT) = EXP( LUM_ADJ(HALF,UNIT)*LUM )
        ENDDO
      ENDDO
      CHANGED = .TRUE.
  999 RETURN
      END
