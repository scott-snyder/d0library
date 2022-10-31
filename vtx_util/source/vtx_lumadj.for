      SUBROUTINE VTX_LUMADJ(GAIN_ADJ,STATUS,CHANGED)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Compute gain adjustment factor due to voltage 
C-           sagging for the first part of RUN IA (thru December, 1992).  Use 
C-           luminosity from special LUMINOSITY.DAT file, together with 
C-           parametrization of I vs. Luminosity.  This correction is based
C-           on CONSTANT SUPPLY VOLTAGE.
C-           
C-           Also -- re-arrange the VDTM linear structure so that the currrent
C-           one is the first one.
C-               
C-
C-   Inputs  : LUMINOSITY.DAT -- ASCII file containg time-stamp and Luminosity
C-   Outputs : GAIN_ADJ  = gain adjustment factors each HV feed -- gains fron 
C-                     VGNL bank will be MULTIPLIED by these factors
C-             LUM       = Luminosity (x10E30)
C-             STATUS = 0 --> OK
C-                      1 --> Luminosity data is stale
C-                     -1 --> Luminosity correction not available
C-             CHANGED     = .TRUE. if calibration has changed, else .FALSE.
C-   Controls: 
C-
C-   Created   2-DEC-1992   Ed Oltman
C-   Updated  20-NOV-1993   Ed Oltman  Avoid oveflow in error message 
C-   Updated  16-FEB-1994   Ed Oltman  Add call to VDYNSAVE 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
c I/O:
      INTEGER STATUS
      LOGICAL CHANGED
      REAL    GAIN_ADJ(2,0:31,0:2)
C Locals:
      LOGICAL FIRST,NO_CORRECTION,LAST_FRESH,NEW
      LOGICAL BYPASS_DBL3_ERROR
      INTEGER ERR,ERR1,TIME_1,LAST_TIME,ERROR
      REAL    XHOU,MAX_TIME,LUM
      CHARACTER*80 TXT
      CHARACTER*1  SEVER
      INTEGER LEN,VAXTIME(2),I
      REAL CURR
      INTEGER NSEC(192),SUPPLY(2,0:31,0:2),NS(0:2)
      INTEGER LAYER,SECTOR,FEED,SUP,DBL3_TIME,LAST_GOOD_TIME
      REAL    PAR(3,192),RESISTOR(6),KR,KR_FACT(6),V_TGT(192)
      INTEGER LAST_RUN
      INTEGER EARLY_TIME
      PARAMETER( EARLY_TIME = 478380083 ) !4-MAR-92 12:08:11
c Externals:
      INTEGER TRULEN,RUNNO
      REAL    D3UXH
c Data:
      DATA FIRST/.TRUE./
      DATA NO_CORRECTION/.FALSE./
      DATA NS/15,31,31/
      DATA LAST_RUN/-1/
C----------------------------------------------------------------------
      IF (FIRST) THEN 
        FIRST = .FALSE.
        CALL VTX_IVLPAR(PAR)
        CALL EZPICK('VTRAKS_RCP')
        CALL EZGET_l('BYPASS_DBL3_ERROR',BYPASS_DBL3_ERROR,ERR)
        ERROR = ERR
        CALL EZGET('LUM_MAX_TIME',MAX_TIME,ERR)
        ERROR = ERROR + ERR
        CALL EZGET_rarr('HV_REFF',RESISTOR,ERR)
        ERROR = ERROR + ERR
        CALL EZGET_rarr('HV_KEFF',KR_FACT,ERR)  !  KEFF(FEED,LAYER)
        ERROR = ERROR + ERR
        DO I = 1,6
          KR_FACT(I) = KR_FACT(I)*RESISTOR(I)
        ENDDO
        CALL EZRSET
        SEVER = 'F'
        IF (BYPASS_DBL3_ERROR) SEVER = 'W'
        IF (ERROR .NE. 0) THEN
          CALL ERRMSG('VTX LUM corr NOT made','VTX_LUMADJ',
     &      'VTRAKS_RCP errors -- cannot apply Lumin based corr.',SEVER)
          NO_CORRECTION = .TRUE.
        ENDIF
      ENDIF
      IF (RUNNO() .NE. LAST_RUN) THEN
        LAST_RUN = RUNNO()
        CALL VTX_HVSLOG(NSEC,SUPPLY,V_TGT)
        LAST_GOOD_TIME = EARLY_TIME
        ERR1 = 0
        LAST_TIME = 0
      ENDIF
C
C ----------------->           NORMAL ENTRY
C
      STATUS = -1
      CHANGED = .FALSE.
      IF (NO_CORRECTION) GO TO 999
      VAXTIME(1) = IQ(LHEAD+4)
      VAXTIME(2) = IQ(LHEAD+5)
      CALL VTX_GETLUM(VAXTIME,DBL3_TIME,LUM,XHOU,TIME_1,ERR)
      CALL VDYNSAVE('LUM',LUM,TIME_1)
      IF (ERR .LT. 0) THEN
        IF (ERR .EQ. -1) THEN
          TXT = 'VAXTIME is illegal'
        ELSEIF(ERR .EQ. -2) THEN
          TXT = 'Requested time too early'
        ELSEIF(ERR .EQ. -3) THEN
          TXT = 'Requested time too late'
        ENDIF
        LEN = TRULEN(TXT)
        TXT = TXT(1:LEN)//' NO LUMINOSITY CORRECTIONS TO BE MADE'
        CALL ERRMSG('VTX LUM corr NOT made','VTX_LUMADJ',TXT(1:LEN),
     &    SEVER)
        STATUS = -1
        NO_CORRECTION = .TRUE.
      ENDIF
      STATUS = 1
c
      LAST_FRESH     = D3UXH(DBL3_TIME,LAST_GOOD_TIME) .LT. MAX_TIME
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
            IF (XHOU .LT. 9999.99) THEN
              WRITE(TXT,'(A,F7.2,A)') 
     &          'LUMINOSITY data is stale (',XHOU,'hours) using anyway'
            ELSE
              TXT = 'DBMON$GLB data is stale (>10000hrs) using anyway'
            ENDIF
            LEN = TRULEN(TXT)
            CALL ERRMSG('VTX LUM corr stale','VTX_LUMADJ',
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
      LAST_GOOD_TIME = LAST_TIME
      LAST_FRESH = XHOU .LT. MAX_TIME
      IF( LAST_FRESH ) THEN
        STATUS = 0
        IF (ERR1 .GT. 0) THEN
          TXT = 'D0LUM data is fresh again'
          LEN = TRULEN(TXT)
          CALL ERRMSG('VTX LUM corr OK','VTX_LUMADJ',TXT(1:LEN),'W')
          ERR1 = 0
        ENDIF
      ELSE
        STATUS = 1
        IF (ERR1 .EQ. 0) THEN
          IF (XHOU .LT. 9999.99) THEN
            WRITE(TXT,'(A,F7.2,A)') 
     &        'LUMINOSITY data is stale (',XHOU,'hours) using anyway'
          ELSE
            TXT = 'DBMON$GLB data is stale (>1000hrs) using anyway'
          ENDIF
          LEN = TRULEN(TXT)
          CALL ERRMSG('VTX LUM corr stale','VTX_LUMADJ',TXT(1:LEN),'W')
        ENDIF
        ERR1 = ERR1 + 1
      ENDIF
C
C ****  OK, NOW COMPUTE GAIN CORRECTION DUE TO SAGGING.  LUM is the luminosity
C ****  corrected for multiple interactions.
C
      DO LAYER = 0,2
        DO SECTOR = 0,NS(LAYER)
          DO FEED = 1,2
            KR = KR_FACT(LAYER*2+FEED)
            SUP = SUPPLY(FEED,SECTOR,LAYER)
            CURR = PAR(1,SUP) + PAR(2,SUP)*LUM + PAR(3,SUP)*LUM**2
            GAIN_ADJ(FEED,SECTOR,LAYER) = EXP(-KR*CURR)
          ENDDO
        ENDDO
      ENDDO
C..update DTM if necessary
      CALL VTX_LUMDTM(LUM)
      CHANGED = .TRUE.
  999 RETURN
      END
