      SUBROUTINE DC_ENVADJ(STATUS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Compute gain adjustment factor and drift velocity
C-           adjustment factor due to pressure and temperature variations. Use
C-           DBMON$GLB and parameterization of gain vs. P.
C-
C-
C-   Inputs  : DBMON$GLB
C-   Outputs : 
C-             STATUS = 0 --> OK
C-                      1 --> DBMON$GLB data is stale
C-                     -1 --> DBMON$GLB correction not available
C-             CHANGED     = .TRUE. if calibration has changed, else .FALSE.
C-   Controls:
C-
C-   Created  14-APR-1993   Paul Rubinov based on VTX_ENVADJ
C-   Updated  30-JUN-1993   Qizhong Li-Demarteau  added EZGET to read switch
C-                                                BYPASS_DBL3_ERROR from RCP
C-   Updated  22-NOV-1993   Paul Rubinov fixed to complie on IBM 
C-
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:DBMUKEY.INC'
      INCLUDE 'D0$INC:DENVADJ.INC'
c I/O:
      INTEGER STATUS
      LOGICAL CHANGED
C Locals:
      LOGICAL FIRST,NO_CORRECTION,LVAR
      LOGICAL LAST_FRESH,GOOD,ONE_GOOD,BYPASS_DBL3_ERROR
      INTEGER ERR,ERR2,ERROR,DBL3_TIME,LASTTIME,LAST_GOOD_TIME
      REAL    XHOU,MAX_TIME,AGE
      INTEGER NDEV,MAXDEV,NUM_DEV,LAST_RUN
      PARAMETER (MAXDEV=4)
      CHARACTER*17 DEV(MAXDEV)
      CHARACTER*1  SEVER
      REAL    VAL(MAXDEV),PABS_LAST
      CHARACTER*80 TXT
      CHARACTER*12 CLASS(MAXDEV)
      INTEGER LEN,VAXTIME(2),I
      INTEGER IND,NWORDS
      REAL    PABS,POFF
      INTEGER EARLY_TIME
      PARAMETER( EARLY_TIME = 478380083 ) !4-MAR-92 12:08:11
      LOGICAL BACKSPACE,OLD
C ENVIORMENTAL STUFF
      REAL    REF_PRESS,DELTA_PRESS
      REAL    AMP_IN,AMP_OUT,CORR_SLOPE_IN,CORR_SLOPE_OUT
c Externals:
      INTEGER TRULEN
      REAL    D3UXH
      LOGICAL D3UPT,D3UUT
c Data:
      DATA FIRST/.TRUE./
      DATA NO_CORRECTION/.FALSE./
      DATA LAST_RUN/-1/
C----------------------------------------------------------------------
      IF (FIRST) THEN
        FIRST = .FALSE.
        ERROR = 0
        CALL EZPICK('DTRAKS_RCP')
        CALL EZGET('BYPASS_DBL3_ERROR',BYPASS_DBL3_ERROR,ERR)
        CALL EZGET('DBM_MAX_TIME',MAX_TIME,ERR)
        ERROR = ERROR + ERR
        CALL EZGET('ENV_NUM_DEV',NUM_DEV,ERR)
        ERROR = ERROR + ERR
C        CALL EZGETA('ENV_DEVICES',0,0,0,NWORDS,ERR)
        ERROR = ERROR + ERR
        NDEV = 0
        IND  = 1
        DO WHILE (NDEV.LT.NUM_DEV)
          NDEV = NDEV + 1
          IF (NDEV .GT. MAXDEV) GO TO 5
          CALL EZGETS('ENV_DEVICES',2*NDEV-1,DEV(NDEV),LEN,ERR)
          ERROR = ERROR + ERR
          CALL EZGETS('ENV_DEVICES',2*NDEV,CLASS(NDEV),LEN,ERR)
          ERROR = ERROR + ERR
        ENDDO
        CALL EZGET('AMP_INNER',AMP_IN,ERR)
        ERROR = ERROR + ERR
        CALL EZGET('AMP_OUTER',AMP_OUT,ERR)
        ERROR = ERROR + ERR
        CALL EZGET('SLOPE_INNER',CORR_SLOPE_IN,ERR)
        ERROR = ERROR + ERR
        CALL EZGET('SLOPE_OUTER',CORR_SLOPE_OUT,ERR)
        ERROR = ERROR + ERR
        CALL EZGET('REF_PRESSURE',REF_PRESS,ERR)
        ERROR = ERROR + ERR
C
    5   CALL EZRSET
        IF (ERROR .NE. 0) THEN
          SEVER = 'F'
          IF (BYPASS_DBL3_ERROR) SEVER = 'W'
          CALL ERRMSG('CDC Pres corr NOT made','DC_ENVADJ',
     &      'DTRAKS_RCP errors -- cannot apply P corrections',SEVER)
          NO_CORRECTION = .TRUE.
        ENDIF
      ENDIF
      ERR2 = 0
      LAST_GOOD_TIME = EARLY_TIME
      ONE_GOOD = .FALSE.
      LASTTIME = 0
C
C ----------------->           NORMAL ENTRY
C
      BACKSPACE = .FALSE.
      STATUS = -1
      CHANGED = .FALSE.
      IF (NO_CORRECTION) GO TO 999
      STATUS = 1
      VAXTIME(1) = IQ(LHEAD+4)
      VAXTIME(2) = IQ(LHEAD+5)
      LVAR = D3UPT(VAXTIME,DBL3_TIME)
   10 NDEV=1
      DO WHILE (NDEV.LT.NUM_DEV)
        CALL DBMU_GETDM(CLASS(NDEV),DEV(NDEV),1,VAXTIME,1,' ',VAL(NDEV),
     &    XHOU,ERR)
        NDEV=NDEV+1
      ENDDO
      IF ((VAL(1).GT.900).AND.(VAL(1).LT.1100)) PABS=VAL(1)
      IF ((VAL(2).GT.900).AND.(VAL(2).LT.1100)) PABS=VAL(2)
      IF (ERR .LT. 0) THEN
        NO_CORRECTION = .TRUE.
        SEVER = 'F'
        IF (BYPASS_DBL3_ERROR) SEVER = 'W'
        CALL ERRMSG('CDC P,T corr NOT made','DC_ENVADJ',
     &    'Cannot attatch to DBMON$GLB -- no P corrections made',
     &    SEVER)
        GO TO 999
      ENDIF
C
C*** LAST_FRESH = .TRUE. --> last good data within MAX_TIME hours
C***     OLD      .FALSE.--> This call to DBMU_GETDM has new data
C
      AGE = D3UXH(DBL3_TIME,LAST_GOOD_TIME)
      LAST_FRESH= AGE .LT. MAX_TIME
      OLD       = DBKEYS(3) .EQ. LASTTIME
      LASTTIME  = DBKEYS(3)
      IF (OLD) THEN
        IF ( .NOT. LAST_FRESH ) THEN
C
C ****  Should be a new record -- existing data is stale
C
          ERR2 = ERR2 + 1
          IF (ERR2 .EQ. 1) THEN
            WRITE(TXT,'(A,G10.2,A)')
     &        'DBMON$GLB data is stale (',AGE,'hours) using anyway'
            LEN = TRULEN(TXT)
            CALL ERRMSG('CDC P corr stale',
     &        'DC_ENVADJ',TXT(1:LEN),'W')
          ENDIF
        ELSE
          STATUS = 0
        ENDIF
        GO TO 999
      ENDIF
C
C ****  This section for NEW record
C
      IF (.NOT. ONE_GOOD) THEN
        GOOD=.FALSE.
        IF (PABS.GT.900) GOOD =.TRUE.
C
C ****      ONE_GOOD = .FALSE ==> no valid (e.g GOOD = .TRUE.) DBL3 record has
C ****      been read for this run.  Start looking backwards in database for
C ****      valid data and use it until there is valid data.
C
        IF (GOOD) THEN
          ONE_GOOD = .TRUE.
          CHANGED = .TRUE.
          IF (BACKSPACE) THEN
            VAXTIME(1) = IQ(LHEAD+4)
            VAXTIME(2) = IQ(LHEAD+5)
            LVAR = D3UPT(VAXTIME,DBL3_TIME)
            XHOU = D3UXH(DBL3_TIME,DBKEYS(3))
          ENDIF
        ELSE
          BACKSPACE = .TRUE.
C
C  *** DONT DO THIS FOR NOW ****
C
C          CALL DBINCT(DBKEYS(3),-10,DBL3_TIME)
C          LVAR = D3UUT(VAXTIME,DBL3_TIME)
C          GO TO 10
          ERR2=0
          ERR2 = ERR2 + 1
          IF (ERR2 .EQ. 1) THEN
            WRITE(TXT,'(A,F7.2,A)')
     &        'DBMON$GLB data is stale (',AGE,'hours) using DEFAULT'
            LEN = TRULEN(TXT)
            CALL ERRMSG('CDC P,T corr not made','DC_ENVADJ',
     &        TXT(1:LEN),'W')
C
C  SET THE PRESSURE TO NOMINAL VALUE
C
            PABS=REF_PRESS
          ENDIF
        ENDIF
      ENDIF
      IF (GOOD) THEN
        LAST_GOOD_TIME = LASTTIME
        LAST_FRESH     = XHOU .LT. MAX_TIME
        AGE = XHOU
      ENDIF
      IF(  LAST_FRESH ) THEN
        STATUS = 0
        IF (ERR2 .GT. 0) THEN
          TXT = 'DBMON$GLB data is fresh again'
          LEN = TRULEN(TXT)
          CALL ERRMSG('CDC P,T corr OK','DC_ENVADJ',
     &      TXT(1:LEN),'W')
          ERR2 = 0
        ENDIF
      ELSE
        STATUS = 1
        ERR2 = ERR2 + 1
        IF (ERR2 .EQ. 1) THEN
          WRITE(TXT,'(A,F7.2,A)')
     &      'DBMON$GLB data is stale (',AGE,'hours) using DEFAULT'
          LEN = TRULEN(TXT)
          CALL ERRMSG('CDC P,T corr not made','DC_ENVADJ',
     &      TXT(1:LEN),'W')
C
C  SET THE PRESSURE TO NOMINAL VALUE
C
          PABS=REF_PRESS
        ENDIF
      ENDIF
C
C ****  OK, if this is good data, save it in case next set is bad; if its bad,
C ****  use the previous set
C
      IF (GOOD) THEN
        PABS_LAST   = PABS
      ELSE
        GOTO 999
      ENDIF
C
C ****  NOW, COMPUTE THE DEALIE-KABOB
C
C*     GFAC IS COMPUTED BASED ON THE REFERENCE PRESSURE AND THE BEST FIT
C*     LINE TO PULSE HEIGHT VERSUS PRESSURE PH=AMP-CORR_SLPOE*PRESS
C
C*     THE NUMBERS ARE KEPT IN THE DTRAKS_RCP
C
      CHANGED = .TRUE.
      POFF=0.
      DELTA_PRESS=PABS-REF_PRESS
      IF ((AMP_IN-CORR_SLOPE_IN*REF_PRESS).NE.0.) THEN
        GFAC_INNER = 1- CORR_SLOPE_IN *
     &    DELTA_PRESS/(AMP_IN-CORR_SLOPE_IN*REF_PRESS)
      ENDIF
      IF ((AMP_OUT-CORR_SLOPE_OUT*REF_PRESS).NE.0.) THEN
        GFAC_OUTER = 1- CORR_SLOPE_OUT *
     &    DELTA_PRESS/(AMP_OUT-CORR_SLOPE_OUT*REF_PRESS)
      ENDIF
      CORRECTION_MADE=.TRUE.      ! IN DENVADJ COMMON BLOCK
C
  999 RETURN
      END
