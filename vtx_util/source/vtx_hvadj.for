      SUBROUTINE VTX_HVADJ(GAIN_ADJUST,STATUS,CHANGED)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Apply "residual" correction to gain factors due to 
C-               Vsupply-IR-Vanode_target # 0.  The VTX currents and voltages
C-               are avialable in DBMON$GLB after 14-Dec-92 18:00 (run 57496)
C-
C-   Inputs  : DBMON$GLB
C-   Outputs : GAIN_ADJUST(feed,sector,layer) = gain adjustment
C-             STATUS = 0 --> ok
C-                      1 --> DBMON$GLB data stale
C-                     -1 --> no correction is available
C-             CHANGED= .TRUE. if calibration has changed, else .FALSE.
C-   Controls: 
C-
C-   Created  10-DEC-1992   Ed Oltman
C-   Updated  12-FEB-1993   Ed Oltman  FIX OVERWRITE BUG 
C-   Updated  27-MAR-1993   Ed Oltman  Backspace through DBL3 if 1st record is
C-                                      not good; update messages,call
C-                                      VTX_LUMDTM(LUM=0) at new run
C-   Updated  30-MAR-1993   Ed Oltman  Treat all 6 HV categories as single
C-                          device -- either all are good/fresh or none are
C-   Updated   7-JUN-1993   Ed Oltman  Rather then backspace, skip forward.. 
C-   Updated  16-OCT-1993   Ed Oltman  Do not skip forward if at end of DBM 
C-   Updated  20-NOV-1993   Ed Oltman  Avoid overflow in error message 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:DBMUKEY.INC'
c I/O:
      REAL    GAIN_ADJUST(2,0:31,0:2)
      INTEGER STATUS
      LOGICAL CHANGED
c Locals:
      LOGICAL FIRST,LVAR,LAST_FRESH,ONE_GOOD,GOOD,NEW
      LOGICAL NO_CORRECTION,BYPASS_DBL3_ERROR
      INTEGER MAX_SUP
      PARAMETER (MAX_SUP = 192)
      INTEGER NSEC(MAX_SUP),SUPPLY(2,0:31,0:2),ERR,LASTTIME
      INTEGER LAST_GOOD_TIME,ERR2,CAT
      INTEGER NEWSCALE_RUN,NCH,SUP,LAYER,SECTOR,FEED,NS(0:2),OFFSET
      CHARACTER*80 TXT
      INTEGER DEVSUP(MAX_SUP),VAXTIME(2),I,ERROR,DBL3_TIME,LEN
      INTEGER IDATE,ITIME,RUN,LAST_RUN
      REAL    ISCALE,SCALES(2),RESISTOR(2,0:2)
      REAL    V_TGT(MAX_SUP),XHOU,KEFF(2,0:2),MAX_TIME,AGE
      CHARACTER*12 DEV(2*MAX_SUP+1),CLASS
      CHARACTER*1  SEVER
      REAL         VAL(2*MAX_SUP+1),REFF(MAX_SUP),DELTA_V(MAX_SUP)
      REAL    DELTA_VLAST(MAX_SUP)
      INTEGER EARLY_TIME
      LOGICAL SKIP,OLD
      PARAMETER( EARLY_TIME = 478380083 ) !4-MAR-92 12:08:11
c External:
      INTEGER RUNNO,TRULEN
      REAL    D3UXH
      LOGICAL D3UPT,D3UUT
c Data:
      DATA FIRST/.TRUE./
      DATA NS/15,31,31/
      DATA NO_CORRECTION/.FALSE./
      DATA CLASS/'DBM_VTX_HV_C'/
      DATA LAST_RUN/-1/
C----------------------------------------------------------------------
      IF (FIRST) THEN
        CALL UFILL(GAIN_ADJUST(1,0,0),1,192,1.)
        FIRST = .FALSE.
        CALL EZPICK('VTRAKS_RCP')
        CALL EZGET('BYPASS_DBL3_ERROR',BYPASS_DBL3_ERROR,ERR)
        ERROR = ERR
        CALL EZGET('HV_MAX_TIME',MAX_TIME,ERR)
        ERROR = ERROR + ERR
        CALL EZGETA('HV_ISCALE',1,1,0,NEWSCALE_RUN,ERR)
        ERROR = ERROR + ERR
        CALL EZGETA('HV_ISCALE',2,3,1,SCALES,ERR)
        ERROR = ERROR + ERR
        CALL EZGET('HV_REFF',RESISTOR(1,0),ERR)
        ERROR = ERROR + ERR
        CALL EZGET('HV_KEFF',KEFF(1,0),ERR)
        ERROR = ERROR + ERR
        CALL EZRSET
        SEVER = 'F'
        IF (BYPASS_DBL3_ERROR) SEVER = 'W'
        IF (ERROR .NE. 0) THEN
          CALL ERRMSG('VTX HV corr NOT made','VTX_HVADJ',
     &      'VTRAKS_RCP errors -- cannot apply HV corrections',SEVER)
          NO_CORRECTION = .TRUE.
          GO TO 999
        ENDIF
      ENDIF
      RUN = RUNNO()
      IF (RUN .NE. LAST_RUN) THEN
        LAST_RUN = RUN
        CALL VTX_HVSLOG(NSEC,SUPPLY,V_TGT)
        ISCALE = 1./SCALES(1)
        IF (RUN .GE. NEWSCALE_RUN) ISCALE = 1./SCALES(2)
        NCH = 0
        DO SUP = 1,MAX_SUP
          IF (NSEC(SUP) .GT. 0) THEN
            WRITE(DEV(NCH+1),'(A,I3.3)') 'VTX_HV.C',SUP
            WRITE(DEV(NCH+2),'(A,I3.3)') 'VTX_HV.V',SUP
            DO LAYER = 0,2
              DO SECTOR = 0,NS(LAYER)
                DO FEED = 1,2
                  IF (SUP .EQ. SUPPLY(FEED,SECTOR,LAYER)) GO TO 10
                ENDDO
              ENDDO
            ENDDO
            CALL ERRMSG('VTX HV corr NOT made','VTX_HVADJ',
     &        'VTRAKS_RCP has inconsistant HV - logical assignments',
     &        SEVER)
   10       REFF(NCH/2+1) = RESISTOR(FEED,LAYER)/NSEC(SUP)
            DEVSUP(NCH/2+1) = SUP
            NCH = NCH + 2
          ENDIF
        ENDDO
        DEV(NCH+1) = 'VTX_HV.LUM'
        ERR2 = 0
        LAST_GOOD_TIME = EARLY_TIME
        ONE_GOOD = .FALSE.
        CALL VTX_LUMDTM(0.)
        LASTTIME = 0
        CALL VZERO(DELTA_VLAST,MAX_SUP)
      ENDIF
C
C ------------------>           NORMAL ENTRY
C
      SKIP = .FALSE.
      STATUS = -1
      CHANGED = .FALSE.
      IF (NO_CORRECTION) GO TO 999
      STATUS =  1
      VAXTIME(1) = IQ(LHEAD+4)
      VAXTIME(2) = IQ(LHEAD+5)
      LVAR = D3UPT(VAXTIME,DBL3_TIME)
C
C ****  Get HV data: organized V(25),I(25),V(26),I(26)...V(192),I(192),LUM
C
   20 CALL DBMU_GETDM(CLASS,DEV,NCH+1,VAXTIME,1,' ',VAL,XHOU,ERR)
      IF (ERR .LT. 0) THEN
        NO_CORRECTION = .TRUE.
        CALL ERRMSG('VTX HV corr NOT made','VTX_HVADJ',
     &    'Cannot attatch to DBMON$GLB -- no HV corrections made',SEVER)
        GO TO 999
      ENDIF
C
C*** LAST_FRESH = .TRUE. --> last good data within MAX_TIME hours
C***     OLD      .FALSE.--> This call to DBMU_GETDM has new data
C
      AGE = D3UXH(DBL3_TIME,LAST_GOOD_TIME)
      LAST_FRESH = AGE .LT. MAX_TIME
      OLD   = DBKEYS(3) .EQ. LASTTIME
      LASTTIME = DBKEYS(3)
C..If old data is stale, generate a warning message
      IF (OLD) THEN 
        IF( .NOT. LAST_FRESH) THEN
C
C ****  Should be a new record -- existing data is stale
C
          ERR2 = ERR2 + 1
          IF (ERR2 .EQ. 1) THEN
            IF (AGE .LT. 9999.9) THEN
              WRITE(TXT,'(A,F7.2,A)') 
     &          'DBMON$GLB data is stale (',AGE,'hours) using anyway'
            ELSE
              TXT = 'DBMON$GLB data is stale (>10000hrs) using anyway'
            ENDIF
            LEN = TRULEN(TXT)
            CALL ERRMSG('VTX HV corr stale',
     &        'VTX_HVADJ', TXT(1:LEN),'W')
          ENDIF
        ELSE
          STATUS = 0
        ENDIF
        GO TO 999
      ENDIF
c..Check HV data; load DELTA_V array with voltage sag
      CALL VTX_HVCHK(VAL,DELTA_V,GOOD)
      IF (.NOT. ONE_GOOD) THEN
C
C ****      ONE_GOOD = .FALSE ==> no valid (e.g GOOD = .TRUE.) DBL3 record has
C ****      been read for this run.  Start looking forwards in database for
C ****      valid data and use it until there is valid data.
C
        IF (GOOD) THEN
          ONE_GOOD = .TRUE.
          CHANGED = .TRUE.
          IF (SKIP) THEN
            VAXTIME(1) = IQ(LHEAD+4)
            VAXTIME(2) = IQ(LHEAD+5)
            LVAR = D3UPT(VAXTIME,DBL3_TIME)
            XHOU = D3UXH(DBL3_TIME,DBKEYS(3))
          ENDIF
        ELSE
          IF (DBKEYS(4) .GT. 999999990) THEN
            CALL ERRMSG('VTX HV corr stale','VTX_HVADJ',
     &        'End of DBMON database -- no correction applied','W')
            GO TO 30
          ENDIF
          SKIP = .TRUE.
          CALL DBINCT(DBKEYS(4),+10,DBL3_TIME)
          LVAR = D3UUT(VAXTIME,DBL3_TIME)
          GO TO 20
        ENDIF
      ENDIF
      IF (GOOD) THEN 
        LAST_GOOD_TIME = LASTTIME
        LAST_FRESH     = XHOU .LT. MAX_TIME
        AGE = XHOU
      ENDIF
      IF (LAST_FRESH) THEN
        STATUS = 0
        IF (ERR2 .GT. 0) THEN
          TXT = 'DBMON$GLB data is fresh again'
          LEN = TRULEN(TXT)
          CALL ERRMSG('VTX HV corr OK','VTX_HVADJ',TXT(1:LEN),'W')
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
          CALL ERRMSG('VTX HV corr stale',
     &        'VTX_HVADJ',TXT(1:LEN),'W')
        ENDIF
      ENDIF
C
C
C
   30 CONTINUE
      DO LAYER = 0,2
        DO SECTOR = 0,NS(LAYER)
          DO FEED = 1,2
            SUP = SUPPLY(FEED,SECTOR,LAYER)
            IF (GOOD) THEN
              DELTA_VLAST(SUP) = DELTA_V(SUP)
              GAIN_ADJUST(FEED,SECTOR,LAYER) = 
     &                EXP(KEFF(FEED,LAYER)*DELTA_V(SUP))
            ELSE
              GAIN_ADJUST(FEED,SECTOR,LAYER) = 
     &                EXP(KEFF(FEED,LAYER)*DELTA_VLAST(SUP))
            ENDIF
          ENDDO
        ENDDO
      ENDDO
      IF (GOOD) CHANGED = .TRUE.
  999 RETURN
      END
