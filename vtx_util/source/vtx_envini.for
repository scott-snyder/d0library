      SUBROUTINE VTX_ENVINI(SCALE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read several drift velocity records (VTX canary) 
C-           from the DBMON database, correct for temperature and pressure and
C-           insert into VDTM
C-
C-   Inputs  : 
C-   Outputs : SCALE -- Non envirnmental correction to drift velocity
C-   Controls: 
C-
C-   Created  25-DEC-1992   Ed Oltman
C-   Updated  27-MAR-1993   Ed Oltman Add USE_CANARY_DEFAULT switch
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:DBMUKEY.INC'
C I/O:
      REAL SCALE
c Locals:
      INTEGER       NDEV
      PARAMETER    (NDEV = 4)
      REAL         VAL(NDEV)
      CHARACTER*17 DEV(NDEV)
      LOGICAL FIRST,BYPASS_DBL3_ERROR,LVAR,DONE,USE_CANARY_DEFAULT
      INTEGER ERR,ERROR,NWORDS,IND,NUMAVE,LEN,TOT,I,J,FAIL
      INTEGER VAXTIME(2),DBL3_TIME,NEWTIME,START_RUN
      REAL    PAR(6),POFF,PEXP,TOFF,TEXP,DEFAULT_VELOCITY
      REAL    MAX_TIME,SERR
      REAL    S1,S2,XHOU,DVEL,PABS,TABS
      CHARACTER*1  SEVER
      CHARACTER*80 TXT
c Externals
      LOGICAL D3UPT
      REAL    D3UXH
      INTEGER TRULEN,RUNNO
c Data:
      EQUIVALENCE (POFF,PAR(1)),(PEXP,PAR(2)),(TOFF,PAR(3)),
     &            (TEXP,PAR(4))
      DATA FIRST/.TRUE./
      DATA DEV/'VTX_CAN_DRFT.VLCY','VTX_CAN_CYCL.RUN',
     &         'VTX_CAN_COND.PABS','VTX_CAN_COND.TMP1'/

C----------------------------------------------------------------------
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL EZPICK('VTRAKS_RCP')
        CALL EZGET_l('BYPASS_DBL3_ERROR',BYPASS_DBL3_ERROR,ERR)
        ERROR = ERR
        CALL EZGET_rarr('ENV_PAR',PAR,ERR)
        ERROR = ERROR + ERR
        CALL EZGET_i('CANARY_AVERAGE',NUMAVE,ERR)
        ERROR = ERROR + ERR
        CALL EZGET('MAX_CANARY_TIME',MAX_TIME,ERR)
        ERROR = ERROR + ERR
        CALL EZGET_i('CANARY_START_RUN',START_RUN,ERR)
        ERROR = ERROR + ERR
        CALL EZGET('CANARY_DEFAULT_VELOCITY',DEFAULT_VELOCITY,ERR)
        ERROR = ERROR + ERR
        CALL EZGET_l('USE_CANARY_DEFAULT',USE_CANARY_DEFAULT,ERR)
        ERROR = ERROR + ERR
        CALL EZRSET
        SEVER = 'F'
        IF (BYPASS_DBL3_ERROR) SEVER = 'W'
        IF (ERROR .NE. 0) THEN
          CALL ERRMSG('VTX SCALE unknown','VTX_ENVINI',
     &      'VTRAKS_RCP errors -- absolute drift velocity unkown',
     &      SEVER)
          USE_CANARY_DEFAULT = .TRUE.
        ENDIF
      ENDIF
C
      IF (RUNNO() .LT. START_RUN .OR. USE_CANARY_DEFAULT) THEN
        CALL ERRMSG('VTX using SCALE default','VTX_ENVINI',
     &    ' Using default drift velocity SCALE -- not canary','W')
        SCALE = DEFAULT_VELOCITY
        SERR = 0.
        GO TO 100
      ENDIF
      VAXTIME(1) = IQ(LHEAD+4)
      VAXTIME(2) = IQ(LHEAD+5)
      LVAR = D3UPT(VAXTIME,DBL3_TIME)
      NEWTIME = DBL3_TIME
      TOT = 0
      FAIL = 0
      DONE = .FALSE.
C
C ****  LOOP OVER SOME CANARY READINGS
C
      S1 = 0.
      S2 = 0.
      DO WHILE ( (TOT .LT. NUMAVE) .AND. (FAIL .LT. 5) )
        FAIL = FAIL + 1
        CALL DBMU_GETDM('DBM_CD',DEV,NDEV,VAXTIME,1,' ',VAL,XHOU, ERR)
        IF (ERR .LT. 0) THEN
          CALL ERRMSG('VTX using SCALE default','VTX_ENVINI',
     &      'Cannot attatch to DBMON$GLB -- Absolute v_drift not found',
     &      SEVER)
          SCALE = DEFAULT_VELOCITY
          GO TO 999
        ENDIF
C..Check validity range and good values
        DONE = D3UXH(DBL3_TIME,DBKEYS(3)) .GT. MAX_TIME
        DO J = 1,NDEV
          IF (VAL(J) .LE. 1.0) GO TO 20
        ENDDO
        FAIL = FAIL - 1
        TOT = TOT + 1
        DVEL = VAL(1)
        PABS = VAL(3) + POFF
        TABS = VAL(4) + TOFF
        SCALE =  DVEL*PABS**PEXP/TABS**TEXP 
        S1 = S1 + SCALE
        S2 = S2 + SCALE**2
   20   CALL DBINCT(DBKEYS(3),-10,NEWTIME) ! subtract 10 secs from beginning
                                           !      of validity rnge
        CALL D3UUT(VAXTIME,NEWTIME)        ! and turn back into a VAX time
      ENDDO
      IF (DONE) THEN
C
C ****  If canary data is from too far in past, use it anyway, but issue 
C ****  warning
C
        WRITE(TXT,'(A,F5.2,A)') ' Canary velocity data is ',
     &         D3UXH(DBL3_TIME,DBKEYS(3))/24.,' days older then data.'
        LEN = TRULEN(TXT)
        CALL ERRMSG('VTX SCALE data late','VTX_ENVINI',
     &    TXT(1:LEN),'W')
      ENDIF
      IF (TOT .GT. 3) THEN
        SCALE = S1/TOT
        SERR  = S2/TOT - SCALE*SCALE
        SERR = SQRT( AMAX1(SERR,0.)/FLOAT(TOT-1) )
      ELSE
        SCALE = DEFAULT_VELOCITY
        SERR = 0.
        CALL ERRMSG('VTX using SCALE default','VTX_ENVINI',
     &    'Problem with database, using default drift velocity','W') 
      ENDIF
  100 CONTINUE
      WRITE(TXT,'(A,F6.3,A,F6.3,A,I3,A,I3)')
     &    ' Initial scale = ',SCALE,'+-',SERR,' found from ',TOT,
     &       ' points.  Requested = ',NUMAVE
      LEN = TRULEN(TXT)
      CALL ERRMSG('VTX velocity SCALE','VTX_ENVINI',TXT(1:LEN),'I')
  999 RETURN
      END
