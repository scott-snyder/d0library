      SUBROUTINE VTX_ENVADJ(GFAC,DFAC,STATUS,CHANGED)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Compute gain adjustment factor and drift velocity
C-           adjustment factor due to pressure and temperature variations. Use  
C-           DBMON$GLB and parameterization of gain and drft velocity vs. T/P.
C-               
C-
C-   Inputs  : DBMON$GLB
C-   Outputs : GFAC -- multiplicative correction to VGNL gain
C-             DFAC -- multiplicative correction to VDTM scale
C-             STATUS = 0 --> OK
C-                      1 --> DBMON$GLB data is stale
C-                     -1 --> DBMON$GLB correction not available
C-             CHANGED     = .TRUE. if calibration has changed, else .FALSE.
C-   Controls: 
C-
C-   Created   5-DEC-1992   Ed Oltman
C-   Updated  27-MAR-1993   Ed Oltman  Backspace through DBL3 if 1st record is
C-                                     not good; update messages
C-   Updated  20-NOV-1993   Ed Oltman  Avoid overflow in error message 
C-   Updated  15-FEB-1994   Ed Oltman  ADD CALLS TO VDYNSAVE 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:DBMUKEY.INC'
c I/O:
      INTEGER STATUS
      LOGICAL CHANGED
      REAL    GFAC,DFAC
C Locals:
      LOGICAL FIRST,NO_CORRECTION,LVAR
      LOGICAL LAST_FRESH,GOOD,ONE_GOOD,NEW,BYPASS_DBL3_ERROR
      INTEGER ERR,ERR2,ERROR,DBL3_TIME,LASTTIME,LAST_GOOD_TIME
      REAL    XHOU,MAX_TIME,AGE
      INTEGER NDEV,MAXDEV
      PARAMETER (MAXDEV=10)
      CHARACTER*17 DEV(MAXDEV),OLDDEV(MAXDEV),NEWDEV(MAXDEV)
      CHARACTER*1  SEVER
      REAL    VAL(MAXDEV),T_DEGC_LAST,PABS_LAST
      CHARACTER*80 TXT
      CHARACTER*12 OLDCLASS,NEWCLASS,CLASS
      INTEGER LEN,VAXTIME(2),I,RUN_NEW
      INTEGER IND,NWORDS,LAST_RUN
      REAL    PABS,T_DEGC,PAR(6),POFF,PEXP,TOFF,TEXP,G0,KG
      INTEGER EARLY_TIME
      PARAMETER( EARLY_TIME = 478380083 ) !4-MAR-92 12:08:11
      LOGICAL BACKSPACE,OLD
c Externals:
      INTEGER TRULEN,RUNNO
      REAL    D3UXH
      LOGICAL D3UPT,D3UUT
c Data:
      EQUIVALENCE (POFF,PAR(1)),(PEXP,PAR(2)),(TOFF,PAR(3)),
     &            (TEXP,PAR(4)),(KG,PAR(5)),(G0,PAR(6))
      DATA FIRST/.TRUE./
      DATA NO_CORRECTION/.FALSE./
      DATA LAST_RUN/-1/
C----------------------------------------------------------------------
      IF (FIRST) THEN 
        FIRST = .FALSE.
        CALL EZPICK('VTRAKS_RCP')
        CALL EZGET('BYPASS_DBL3_ERROR',BYPASS_DBL3_ERROR,ERR)
        ERROR = ERR
        CALL EZGET('DBM_MAX_TIME',MAX_TIME,ERR)
        ERROR = ERROR + ERR
        CALL EZGET('ENV_PAR',PAR,ERR)
        ERROR = ERROR + ERR
        CALL EZGET('LUM_STOP_RUN',RUN_NEW,ERR)
        ERROR = ERROR + ERR
        CALL EZGETA('ENV_DEVICES',0,0,0,NWORDS,ERR)
        ERROR = ERROR + ERR
        NDEV = 0
        IND  = 1
        DO WHILE (IND .LT. NWORDS)
          NDEV = NDEV + 1
          IF (NDEV .GT. MAXDEV) GO TO 5
          CALL EZGETS('ENV_DEVICES',2*NDEV-1,OLDDEV(NDEV),LEN,ERR)
          ERROR = ERROR + ERR
          IND = IND + (LEN+3)/4
          CALL EZGETS('ENV_DEVICES',2*NDEV  ,NEWDEV(NDEV),LEN,ERR)
          ERROR = ERROR + ERR
          IND = IND + (LEN+3)/4
        ENDDO
    5   IF (NDEV .NE. MAXDEV) ERROR = ERROR + 1
        CALL EZGETS('ENV_CLASS',1,OLDCLASS,LEN,ERR)
        ERROR = ERROR + ERR
        CALL EZGETS('ENV_CLASS',2,NEWCLASS,LEN,ERR)
        ERROR = ERROR + ERR
        CALL EZRSET
        IF (ERROR .NE. 0) THEN
          SEVER = 'F'
          IF (BYPASS_DBL3_ERROR) SEVER = 'W'
          CALL ERRMSG('VTX P,T corr NOT made','VTX_ENVADJ',
     &      'VTRAKS_RCP errors -- cannot apply P,T corrections',SEVER)
          NO_CORRECTION = .TRUE.
        ENDIF
      ENDIF
      IF (RUNNO() .NE. LAST_RUN) THEN
        LAST_RUN = RUNNO()
        NEW =  LAST_RUN .GE. RUN_NEW
        DO I = 1,NDEV
          DEV(I) = OLDDEV(I)
          IF (NEW) DEV(I) = NEWDEV(I)
        ENDDO
        CLASS = OLDCLASS
        IF (NEW) CLASS = NEWCLASS
        ERR2 = 0
        LAST_GOOD_TIME = EARLY_TIME
        ONE_GOOD = .FALSE.
        LASTTIME = 0
      ENDIF
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
   10 CALL DBMU_GETDM(CLASS,DEV,NDEV,VAXTIME,1,' ',VAL,XHOU,ERR)
      IF (ERR .LT. 0) THEN
        NO_CORRECTION = .TRUE.
        SEVER = 'F'
        IF (BYPASS_DBL3_ERROR) SEVER = 'W'
        CALL ERRMSG('VTX P,T corr NOT made','VTX_ENVADJ',
     &    'Cannot attatch to DBMON$GLB -- no P,T corrections made',
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
            IF (AGE .LT. 9999.99) THEN
              WRITE(TXT,'(A,F7.2,A)') 
     &          'DBMON$GLB data is stale (',AGE,'hours) using anyway'
            ELSE
              TXT = 'DBMON$GLB data is stale (>10000hrs) using anyway'
            ENDIF
            LEN = TRULEN(TXT)
            CALL ERRMSG('VTX P,T corr stale',
     &        'VTX_ENVADJ',TXT(1:LEN),'W')
          ENDIF
        ELSE
          STATUS = 0
        ENDIF
        GO TO 999
      ENDIF
C
C ****  This section for NEW record
C
      CALL VTX_ENVCHK(VAL,PABS,T_DEGC,GOOD)
      CALL VDYNSAVE('PABS',PABS,LASTTIME)
      CALL VDYNSAVE('TDEGC',T_DEGC,LASTTIME)
      IF (.NOT. ONE_GOOD) THEN
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
          CALL ERRMSG('VTX P,T corr OK','VTX_ENVADJ',
     &      TXT(1:LEN),'W')
          ERR2 = 0
        ENDIF
      ELSE
        STATUS = 1
        ERR2 = ERR2 + 1
        IF (ERR2 .EQ. 1) THEN
          IF (AGE .LT. 9999.99) THEN
            WRITE(TXT,'(A,F7.2,A)') 
     &        'DBMON$GLB data is stale (',AGE,'hours) using anyway'
          ELSE
            TXT = 'DBMON$GLB data is stale (>1000hrs) using anyway'
          ENDIF
          LEN = TRULEN(TXT)
          CALL ERRMSG('VTX P,T corr stale','VTX_ENVADJ',
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
      ELSE
        GOTO 999
      ENDIF
C
C ****  NOW, COMPUTE THE DEALIE-KABOB
C
      CHANGED = .TRUE.
      DFAC = (T_DEGC+TOFF)**TEXP/(PABS+POFF)**PEXP
      GFAC = G0*EXP( KG*(T_DEGC+TOFF)/(PABS+POFF) )
  999 RETURN
      END
