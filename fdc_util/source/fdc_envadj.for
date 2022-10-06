      SUBROUTINE FDC_ENVADJ(GFAC,STATUS,CHANGED)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Compute gain adjustment factor 
C-           adjustment factor due to pressure and temperature variations. Use  
C-           DBMON$GLB and parameterization of gain and drft velocity vs. T/P.
C-               
C-
C-   Inputs  : DBMON$GLB
C-   Outputs : GFAC -- multiplicative correction to VGNL gain
C-             STATUS = 0 --> OK
C-                      1 --> DBMON$GLB data is stale
C-                     -1 --> DBMON$GLB correction not available
C-             CHANGED     = .TRUE. if calibration has changed, else .FALSE.
C-   Controls: 
C-
C-   Created   5-APR-1993   Robert E. Avery  Based on VTX_ENVADJ by Ed Oltman
C-   Updated  10-OCT-1994   Srini Rajagopalan  Separate out Temperture and
C-   pressure devices, Allow averaging of any number of temp/pressure devices
C-   through RCP control. Change in argument to FDC_ENVCHK
C-   Also check if DBMON returns the actual number of devices requested.
C-   Updated  25-MAR-1995   Yi-Cheng Liu  Get rid of redundant variables 
C-                          GOODTIME,BADTIME,I,NWORDS, and initialize 
C-                          ERR2 before use.
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:DBMUKEY.INC'
c I/O:
      INTEGER STATUS
      LOGICAL CHANGED
      REAL    GFAC
C Locals:
      LOGICAL FIRST,NO_CORRECTION,LVAR
      LOGICAL LAST_FRESH,GOOD,ONE_GOOD,BYPASS_DBL3_ERROR
      INTEGER ERR,ERR2,ERROR,DBL3_TIME,LASTTIME,LAST_GOOD_TIME
      REAL    XHOU,MAX_TIME,AGE
      INTEGER NDEV,NDEVT,NDEVP,MAXDEV
      PARAMETER (MAXDEV=15)
      CHARACTER*17 DEV(MAXDEV)
      CHARACTER*1  SEVER
      REAL    VAL(MAXDEV),T_DEGC_LAST,PABS_LAST
      CHARACTER*80 TXT
      CHARACTER*12 CLASS
      INTEGER LEN,VAXTIME(2)
      INTEGER IND,LAST_RUN
      REAL    PABS,T_DEGC,PAR(4),POFF,TOFF,G0,KG
      INTEGER EARLY_TIME
      PARAMETER( EARLY_TIME = 478380083 ) !4-MAR-92 12:08:11
      LOGICAL BACKSPACE,OLD
c Externals:
      INTEGER TRULEN,RUNNO
      REAL    D3UXH
      LOGICAL D3UPT,D3UUT
c Data:
      EQUIVALENCE (POFF,PAR(1)),(TOFF,PAR(2)),(KG,PAR(3)),(G0,PAR(4))
      DATA FIRST/.TRUE./
      DATA NO_CORRECTION/.FALSE./
      DATA LAST_RUN/-1/
      DATA CLASS / 'DBM_CD'/
C----------------------------------------------------------------------
      IF (FIRST) THEN 
        FIRST = .FALSE.
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET_l('BYPASS_DBL3_ERROR',BYPASS_DBL3_ERROR,ERR)
        ERROR = ERR
        CALL EZGET('DBM_MAX_TIME',MAX_TIME,ERR)
        ERROR = ERROR + ERR
        CALL EZGET_rarr('ENV_PAR',PAR,ERR)
        ERROR = ERROR + ERR
C
        CALL EZGET_NUMBER_STRINGS('ENV_TEMP_DEVICES',NDEVT,ERR)
        ERROR = ERROR + ERR
        CALL EZGET_NUMBER_STRINGS('ENV_PRES_DEVICES',NDEVP,ERR)
        ERROR = ERROR + ERR
        NDEV = NDEVP+NDEVT
        IF (NDEV.GT.MAXDEV) ERROR = ERROR + 1
C
        IF (ERROR.EQ.0) THEN
          DO IND = 1,NDEVT
            CALL EZGETS('ENV_TEMP_DEVICES',IND,DEV(IND),LEN,ERR)
            ERROR = ERROR + ERR
          ENDDO
C
          DO IND = 1,NDEVP
            CALL EZGETS('ENV_PRES_DEVICES',IND,DEV(IND+NDEVT),LEN,ERR)
            ERROR = ERROR + ERR
          ENDDO
        ENDIF
C
        CALL EZRSET
C
        IF (ERROR .NE. 0) THEN
          SEVER = 'F'
          IF (BYPASS_DBL3_ERROR) SEVER = 'W'
          CALL ERRMSG('FDC P,T corr NOT made','FDC_ENVADJ',
     &      'FTRAKS_RCP errors -- cannot apply P,T corrections',SEVER)
          NO_CORRECTION = .TRUE.
        ENDIF
      ENDIF
      IF (RUNNO() .NE. LAST_RUN) THEN
        LAST_RUN = RUNNO()
        LAST_GOOD_TIME = EARLY_TIME
        ONE_GOOD = .FALSE.
        LASTTIME = 0
      ENDIF
C
C ----------------->           NORMAL ENTRY
C
C
      ERR2 = 0
C
      BACKSPACE = .FALSE.
      STATUS = -1
      CHANGED = .FALSE.
      IF (NO_CORRECTION) GO TO 999
      STATUS = 1
      VAXTIME(1) = IQ(LHEAD+4)
      VAXTIME(2) = IQ(LHEAD+5)
      LVAR = D3UPT(VAXTIME,DBL3_TIME)
   10 CALL DBMU_GETDM(CLASS,DEV,NDEV,VAXTIME,1,' ',VAL,XHOU,ERR)
      IF (ERR .LT. 0) THEN
        NO_CORRECTION = .TRUE.
        SEVER = 'F'
        IF (BYPASS_DBL3_ERROR) SEVER = 'W'
        CALL ERRMSG('FDC P,T corr NOT made','FDC_ENVADJ',
     &    'Cannot attatch to DBMON$GLB -- no P,T corrections made',
     &    SEVER)
        GO TO 999
      ELSE IF (ERR .NE. NDEV) THEN
        CALL ERRMSG('All requested env. devices not available',
     &    'FDC_ENVADJ','DBMON returns fewer devices','W')
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
            WRITE(TXT,'(A,F7.2,A)') 
     &        'DBMON$GLB data is stale (',AGE,'hours) using anyway'
            LEN = TRULEN(TXT)
            CALL ERRMSG('FDC P,T corr stale',
     &        'FDC_ENVADJ',TXT(1:LEN),'W')
          ENDIF
        ELSE
          STATUS = 0
        ENDIF
        GO TO 999
      ENDIF
C
C ****  This section for NEW record
C
      CALL FDC_ENVCHK(VAL,NDEVT,NDEVP,PABS,T_DEGC,GOOD)
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
          CALL ERRMSG('FDC P,T corr OK','FDC_ENVADJ',
     &      TXT(1:LEN),'W')
          ERR2 = 0
        ENDIF
      ELSE
        STATUS = 1
        ERR2 = ERR2 + 1
        IF (ERR2 .EQ. 1) THEN
          WRITE(TXT,'(A,F7.2,A)') 
     &      'DBMON$GLB data is stale (',AGE,'hours) using anyway'
          LEN = TRULEN(TXT)
          CALL ERRMSG('FDC P,T corr stale','FDC_ENVADJ',
     &      TXT(1:LEN),'W')
        ENDIF
      ENDIF
C
C ****  OK, if this is good data, save it in case next set is bad; if its bad,
C ****  use the previous set
C
      IF (GOOD) THEN
        T_DEGC_LAST = T_DEGC
        PABS_LAST   = PABS
C
C ****  NOW, COMPUTE THE CORRECTION FACTOR
C
        CHANGED = .TRUE.
        GFAC = G0*EXP( KG*(T_DEGC+TOFF)/(PABS+POFF) )
        GFAC = 1./GFAC 
      ENDIF
C
  999 RETURN
      END
