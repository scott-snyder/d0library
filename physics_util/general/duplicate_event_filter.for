      LOGICAL FUNCTION DUPLICATE_EVENT_FILTER()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Filter duplicate events based on run and
C-                         event number.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Entry points:  DUPLICATE_EVENT_FILTER_INI - Initialization.
C-                  DUPLICATE_EVENT_FILTER_END - Statistical summary.
C-
C-   Created  10-Dec-1994   Herbert Greenlee
C-   Modified 21-Dec-1995   Joey Thompson  Make new RZ file if update requested
C-                                          but no existing file found
C-   Modified 13-Apr-1996   Herbert Greenlee - Revamped logic to fix bug
C-                                             in updating mode.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL DUPLICATE_EVENT_FILTER_INI, DUPLICATE_EVENT_FILTER_END
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:QUEST.INC'
C-
C- Variables from DUPLICATE_EVENT_FILTER_RCP.
C-
      LOGICAL DO_DUPLICATE_EVENT_FILTER
      INTEGER NUM_RUN_EVENT_RZ
      CHARACTER*256 RUN_EVENT_RZ
      LOGICAL FILTER_MONTECARLO
C-
C- RZ file variables
C-
      CHARACTER*8 DUMMY
      CHARACTER*8 CHTAG(5)
      INTEGER RZUNIT, LRECL, ALLOC
      CHARACTER*8 TOPDIR, TOPPATH
      LOGICAL UPDATE_RZFILE, MAKE_NEW_RZFILE
C-
C- Key veriables
C-
      INTEGER NUM_KEY, MAX_KEY, DATA_SIZE
      PARAMETER(NUM_KEY=5, MAX_KEY=2000, DATA_SIZE=10000)
      INTEGER DATA_LEN, NEW_LEN
      INTEGER ICYCLE
      INTEGER NKEY                          ! Actual number of keys in dir.
      INTEGER CURRENT_KEY, NEW_KEY, IKEY
      INTEGER ILOW, IHIGH
      INTEGER CURRENT_KEY_STAT              ! Status of current key in memory:
                                            ! 0 = Not valid in memory
                                            ! 1 = Valid in memory only.
                                            ! 2 = Valid in memory and disk.
      INTEGER KEY(NUM_KEY, MAX_KEY)         ! Keys associated with top dir.
      INTEGER EVBUF(2, DATA_SIZE)           ! Data buffer (run, event)
C-
C- Statistics
C-
      INTEGER NUM_EVENT_INPUT, NUM_EVENT_OUTPUT
C-
C- Other variables and functions.
C-
      INTEGER MIN_RUN, MAX_RUN, MIN_EVENT, MAX_EVENT
      PARAMETER(MIN_RUN = -100000000, MAX_RUN = 100000000)
      PARAMETER(MIN_EVENT = -100000000, MAX_EVENT = 100000000)
      INTEGER IER
      INTEGER I, L, N
      INTEGER RUN, EVENT, RUNNO, EVONUM
      CHARACTER*80 MSG
      INTEGER LUN, SSUNIT, TRULEN
      LOGICAL MCDATA
      LOGICAL FIRST, RZ_FILE_OPEN
C-
      DATA FIRST/.TRUE./
      DATA CHTAG/'NUM_EV', 'MIN_RUN', 'MIN_EV', 'MAX_RUN', 'MAX_EV'/
C----------------------------------------------------------------------
      DUPLICATE_EVENT_FILTER = .TRUE.
      NUM_EVENT_INPUT = NUM_EVENT_INPUT + 1
      MCDATA = IQ(LHEAD+1).GT.1000           ! Monte Carlo data?
      RUN = RUNNO()
      EVENT = EVONUM()
      IF(RUN.LT.MIN_RUN .OR. RUN.GT.MAX_RUN .OR.
     &   EVENT.LT.MIN_EVENT .OR. EVENT.GT.MAX_EVENT)THEN
        CALL ERRMSG('Bad run/event', 'DUPLICATE_EVENT_FILTER',
     &      'Run or event number is out of range', 'W')
        GO TO 998
      ENDIF
C-
C- Turn off processing for Monte Carlo data, unless specifically requested.
C-
      IF(MCDATA.AND..NOT.FILTER_MONTECARLO)
     &  DO_DUPLICATE_EVENT_FILTER = .FALSE.
C-
C- Test for duplicate event
C-
      IF(DO_DUPLICATE_EVENT_FILTER)THEN
C-
C- Open RZ file if not already open.
C-
        IF(.NOT.RZ_FILE_OPEN)THEN
          RZ_FILE_OPEN = .TRUE.
          CALL GTUNIT(777, RZUNIT, IER)
          IF(IER.NE.0)CALL ERRMSG('GTUNIT error', 
     &      'DUPLICATE_EVENT_FILTER', 'Error getting UNIT', 'F')
          L = TRULEN(RUN_EVENT_RZ)
          IF(MAKE_NEW_RZFILE)THEN
            CALL RZOPEN(RZUNIT, DUMMY, RUN_EVENT_RZ(1:L), 'N1', LRECL, 
     &        IER)
          ELSEIF(UPDATE_RZFILE)THEN
            CALL RZOPEN(RZUNIT, DUMMY, RUN_EVENT_RZ(1:L), '1', LRECL, 
     &        IER)
C- Make new RZ file if updated one not found
            IF(IER.NE.0) THEN
              CALL ERRMSG('RZopen error','DUPLICATE_EVENT_FILTER',
     &             'RZ file not found - creating new file','W')
              IER = 0
              CALL RZOPEN(RZUNIT, DUMMY, RUN_EVENT_RZ(1:L), 'N1', LRECL, 
     &          IER)
              MAKE_NEW_RZFILE = .TRUE.
            ENDIF
          ELSE
            CALL RZOPEN(RZUNIT, DUMMY, RUN_EVENT_RZ(1:L), ' ', LRECL, 
     &        IER)
          ENDIF
          IF(IER.NE.0)CALL ERRMSG('RZOPEN error', 
     &      'DUPLICATE_EVENT_FILTER', 'Error from RZOPEN', 'F')
          WRITE(TOPDIR,'(''LUN'',I3.3)')RZUNIT
          TOPPATH = '//'//TOPDIR
C-
C- Declare old RZ file (RZFILE) or make new one (RZMAKE).
C-
          IF(MAKE_NEW_RZFILE)THEN
            CALL RZMAKE(RZUNIT, TOPDIR, 5, 'IIIII', CHTAG, ALLOC, ' ')
            IF(IQUEST(1).NE.0)GO TO 804
C- Make an initial empty key covering all possible run and event numbers.
            NKEY = 1
            CURRENT_KEY = 1
            CURRENT_KEY_STAT = 1
            KEY(1, CURRENT_KEY) = 0 ! Number of entries.
            KEY(2, CURRENT_KEY) = MIN_RUN ! Minimum run number
            KEY(3, CURRENT_KEY) = MIN_EVENT ! Minimum event number
            KEY(4, CURRENT_KEY) = MAX_RUN ! Maximum run number
            KEY(5, CURRENT_KEY) = MAX_EVENT ! Maximum event number
            CURRENT_KEY_STAT = 1
          ELSE
            IF(UPDATE_RZFILE)THEN
              CALL RZFILE(RZUNIT, TOPDIR, '1')
            ELSE
              CALL RZFILE(RZUNIT, TOPDIR, ' ')
            ENDIF
            IF(IQUEST(1).NE.0)GO TO 806
C- Read keys.
            CALL RZKEYS(NUM_KEY, MAX_KEY, KEY, NKEY)
            IF(IQUEST(1).NE.0)GO TO 803
            CURRENT_KEY = 0
            CURRENT_KEY_STAT = 0
          ENDIF
        ENDIF
C-
C- RZ file is open.  Go to top directory.
C-
        CALL RZCDIR(TOPPATH, ' ')
        IF(IQUEST(1).NE.0)GO TO 802
C-
C- Find the key that matches the current run/event number.  It is a fatal 
C- error if there is no matching key.
C-
        NEW_KEY = 0
        DO IKEY = 1, NKEY
          IF( (RUN.GT.KEY(2,IKEY) .OR. 
     &        (RUN.GE.KEY(2,IKEY) .AND. EVENT.GE.KEY(3,IKEY)))
     &        .AND. 
     &        (RUN.LT.KEY(4,IKEY) .OR.
     &        (RUN.LE.KEY(4,IKEY) .AND. EVENT.LE.KEY(5,IKEY))))THEN
            NEW_KEY = IKEY
            GO TO 100
          ENDIF
        ENDDO
 100    CONTINUE
        IF(NEW_KEY.EQ.0)THEN
          CALL ERRMSG('No Key', 'DUPLICATE_EVENT_FILTER',
     &        'No key is appropriate for thie event', 'F')
          GO TO 998
        ENDIF
C-
C- Make sure that the correct key is in memory.  This may involve 
C- writing the current key in memory to disk or reading a key from disk.
C-
        IF(NEW_KEY.NE.CURRENT_KEY)THEN
          IF(CURRENT_KEY_STAT.EQ.1.AND.UPDATE_RZFILE)THEN
            DATA_LEN = KEY(1, CURRENT_KEY)
            CALL RZVOUT(EVBUF, 2*DATA_LEN, KEY(1, CURRENT_KEY), 
     &        ICYCLE, 'I')
            IF(IQUEST(1).NE.0)GO TO 801
          ENDIF
          CALL RZVIN(EVBUF, 2*DATA_SIZE, N, KEY(1,NEW_KEY), 
     &      1, 'D')
          IF(N.NE.2*KEY(1,NEW_KEY))THEN
            CALL ERRMSG('Wrong size', 'DUPLICATE_EVENT_FILTER',
     &        'Size/key mismatch', 'F')
            GO TO 998
          ENDIF
          IF(IQUEST(1).NE.0)GO TO 800
          CURRENT_KEY = NEW_KEY
          CURRENT_KEY_STAT = 2
        ENDIF
C-
C- Now we have the right key in memory.  Do a binary search to find if 
C- the current event matches any in the list, and if not where to insert 
C- the new event.
C-
        DATA_LEN = KEY(1,CURRENT_KEY)
        ILOW = 0
        IHIGH = DATA_LEN + 1
        DO WHILE(IHIGH-ILOW .GT. 1)
          I = (ILOW + IHIGH)/2
          IF(RUN.EQ.EVBUF(1,I) .AND. EVENT.EQ.EVBUF(2,I))THEN
            WRITE(MSG,
     &        '(''Duplicate event found: run'',I6,'', event'',I6)')
     &        RUN, EVENT
            CALL ERRMSG('Duplicate event', 'DUPLICATE_EVENT_FILTER',
     &        MSG, 'W')
            DUPLICATE_EVENT_FILTER = .FALSE.
            GO TO 998
          ELSEIF(RUN.GT.EVBUF(1,I) .OR. 
     &          (RUN.GE.EVBUF(1,I).AND.EVENT.GT.EVBUF(2,I)))THEN
            ILOW = I
          ELSE
            IHIGH = I
          ENDIF
        ENDDO
C-
C- No matches were found (return value will be .true.).  If we are in
C- update mode, then insert the current event into the current key.
C-
        IF(UPDATE_RZFILE)THEN
C-
C- First check to see if the current key in memory as at the its maximum 
C- size.  If it is, cut it in half and write the half that does not contain
C- the current event to disk.
C-
          DATA_LEN = KEY(1,CURRENT_KEY)
          IF(DATA_LEN.GE.DATA_SIZE)THEN
            IF(CURRENT_KEY_STAT.EQ.2)THEN
              CALL RZDELK(KEY(1, CURRENT_KEY), 0, 'C')
              IF(IQUEST(1).NE.0)GO TO 805
            ENDIF
            IF(ILOW.GT.DATA_LEN/2)THEN
              NEW_LEN = ILOW
            ELSE
              NEW_LEN = IHIGH
            ENDIF
            NKEY = NKEY + 1
            IF(NKEY.GT.MAX_KEY)THEN
              CALL ERRMSG('Too many keys', 'DUPLICATE_EVENT_FILTER',
     &          'No more space for additional keys', 'F')
              GO TO 998
            ENDIF
            KEY(1, NKEY) = DATA_LEN - NEW_LEN
            KEY(2, NKEY) = EVBUF(1,NEW_LEN)
            KEY(3, NKEY) = EVBUF(2,NEW_LEN) + 1
            KEY(4, NKEY) = KEY(4, CURRENT_KEY)
            KEY(5, NKEY) = KEY(5, CURRENT_KEY)
            KEY(1, CURRENT_KEY) = NEW_LEN
            KEY(4, CURRENT_KEY) = EVBUF(1,NEW_LEN)
            KEY(5, CURRENT_KEY) = EVBUF(2,NEW_LEN)
C-
C- Write one of CURRENT_KEY or NKEY to disk.  The other key will become
C- the new CURRENT_KEY.
C-
            IF( (RUN.GT.KEY(2,NKEY) .OR. 
     &          (RUN.GE.KEY(2,NKEY) .AND. EVENT.GE.KEY(3,NKEY)))
     &          .AND. 
     &          (RUN.LT.KEY(4,NKEY) .OR.
     &          (RUN.LE.KEY(4,NKEY) .AND. EVENT.LE.KEY(5,NKEY))))THEN
C-
C- Save NKEY, write CURRENT_KEY.
C-
              CALL RZVOUT(EVBUF, 2*KEY(1, CURRENT_KEY), 
     &          KEY(1, CURRENT_KEY), ICYCLE, 'I')
              IF(IQUEST(1).NE.0)GO TO 801
              DO I = 1, DATA_LEN - NEW_LEN
                EVBUF(1,I) = EVBUF(1,I+NEW_LEN)
                EVBUF(2,I) = EVBUF(2,I+NEW_LEN)
              ENDDO
              IHIGH = IHIGH - NEW_LEN
              CURRENT_KEY = NKEY
            ELSE
C-
C- Save CURRENT_KEY, write NKEY.
C-
              CALL RZVOUT(EVBUF(1,NEW_LEN+1), 2*KEY(1, NKEY), 
     &          KEY(1, NKEY), ICYCLE, 'I')
              IF(IQUEST(1).NE.0)GO TO 801
            ENDIF
            CURRENT_KEY_STAT = 1
          ENDIF
C-
C- Having checked the current key for space, and expanded it if necesary, 
C- here is where we actually insert the new event.
C-
          DATA_LEN = KEY(1, CURRENT_KEY)
          DO I = DATA_LEN, IHIGH, -1
            EVBUF(1,I+1) = EVBUF(1,I)
            EVBUF(2,I+1) = EVBUF(2,I)
          ENDDO
          EVBUF(1,IHIGH) = RUN
          EVBUF(2,IHIGH) = EVENT
          DATA_LEN = DATA_LEN + 1
          IF(CURRENT_KEY_STAT.EQ.2)THEN
            CALL RZDELK(KEY(1, CURRENT_KEY), 0, 'C')
            IF(IQUEST(1).NE.0)GO TO 805
          ENDIF
          CURRENT_KEY_STAT = 1
          KEY(1, CURRENT_KEY) = DATA_LEN
        ENDIF
      ENDIF
C-
C- Event statistics
C-
 998  CONTINUE
      IF(DUPLICATE_EVENT_FILTER)NUM_EVENT_OUTPUT = NUM_EVENT_OUTPUT + 1
      GO TO 999
 
      ENTRY DUPLICATE_EVENT_FILTER_INI()
C-
C- Initialization entry point
C-
      DUPLICATE_EVENT_FILTER_INI = .TRUE.
      IF(FIRST) THEN
C-
C- Zero statistics
C-
        NUM_EVENT_INPUT = 0
        NUM_EVENT_OUTPUT = 0
        RZ_FILE_OPEN = .FALSE.
C-
C- Read RCP parameters.  First read from DUPLICATE_EVENT_FILTER_RCP.
C-
        CALL EZPICK_NOMSG('DUPLICATE_EVENT_FILTER_RCP', IER)
        IF(IER.NE.0)THEN
          CALL INRCP('DUPLICATE_EVENT_FILTER_RCP', IER)
          CALL EZPICK_NOMSG('DUPLICATE_EVENT_FILTER_RCP', IER)
        ENDIF
        IF(IER.EQ.0)CALL EZGET('DO_DUPLICATE_EVENT_FILTER',
     &    DO_DUPLICATE_EVENT_FILTER, IER)
        IF(IER.EQ.0.AND.DO_DUPLICATE_EVENT_FILTER)THEN
          IF(IER.EQ.0)CALL EZGET('FILTER_MONTECARLO', FILTER_MONTECARLO,
     &      IER)
          IF(IER.EQ.0)CALL EZGET('PRIMARY_ALLOCATION', ALLOC, IER)
          IF(IER.EQ.0)CALL EZGET('RECORD_LENGTH', LRECL, IER)
          IF(IER.EQ.0)CALL EZGET('UPDATE_RZFILE', UPDATE_RZFILE, IER)
          IF(IER.EQ.0)CALL EZGET('MAKE_NEW_RZFILE', MAKE_NEW_RZFILE, 
     &      IER)
          IF(IER.EQ.0)CALL EZ_GET_CHARS('RUN_EVENT_RZ', 
     &      NUM_RUN_EVENT_RZ, RUN_EVENT_RZ, IER)
        ENDIF
        CALL EZRSET
        IF (IER.NE.0) CALL ERRMSG('Error in DUPLICATE_EVENT_FILTER_RCP',
     &    'DUPLICATE_EVENT_FILTER_INI',' ','F')
        FIRST=.FALSE.
      ENDIF
      UPDATE_RZFILE = UPDATE_RZFILE .OR. MAKE_NEW_RZFILE ! New implies update
      GO TO 999
 
      ENTRY DUPLICATE_EVENT_FILTER_END()
C-
C- Job summary entry point.  First close RZ file.
C-
      IF(DO_DUPLICATE_EVENT_FILTER.AND.UPDATE_RZFILE)THEN
        CALL RZCDIR(TOPPATH, ' ')
        IF(IQUEST(1).NE.0)GO TO 802
        IF(CURRENT_KEY_STAT.EQ.1)THEN
          DATA_LEN = KEY(1, CURRENT_KEY)
          CALL RZVOUT(EVBUF, 2*DATA_LEN, KEY(1, CURRENT_KEY), 
     &      ICYCLE, 'I')
          IF(IQUEST(1).NE.0)GO TO 801
        ENDIF
        CALL RZCLOS(TOPDIR, ' ')
      ENDIF
      LUN = SSUNIT()
      PRINT 500, NUM_EVENT_INPUT, NUM_EVENT_OUTPUT
      WRITE(LUN,500)NUM_EVENT_INPUT, NUM_EVENT_OUTPUT
 500  FORMAT(/' DUPLICATE_EVENT_FILTER package statistics'/
     &  /1X,I8,' Events processed'
     &  /1X,I8,' Events selected'/)
      GO TO 999
C-
C- Fatal RZ errors handled below:
C-
C- RZVIN error.
C-
 800  CONTINUE
      CALL ERRMSG('RZVIN failed', 'DUPLICATE_EVENT_FILTER', 
     &  'Error reading RZ file', 'F')
      GO TO 999
C-
C- RZVOUT error.
C-
 801  CONTINUE
      CALL ERRMSG('RZVOUT failed', 'DUPLICATE_EVENT_FILTER', 
     &  'Error writing RZ file', 'F')
      GO TO 999
C-
C- RZCDIR error.
C-
 802  CONTINUE
      MSG = 'Error setting directory to'//TOPDIR
      CALL ERRMSG('RZCDIR failed', 'DUPLICATE_EVENT_FILTER', 
     &  MSG, 'F')
      GO TO 999
C-
C- RZKEYS error.
C-
 803  CONTINUE
      MSG = 'Error getting keys for '//TOPDIR
      CALL ERRMSG('RZKEYS failed', 'DUPLICATE_EVENT_FILTER',
     &  MSG, 'F')
      GO TO 999
C-
C- RZMAKE error.
C-
 804  CONTINUE
      CALL ERRMSG('RZMAKE failed', 'DUPLICATE_EVENT_FILTER_INI',
     &  'Error initializing RZ file', 'F')
      GO TO 999
C-
C- RZDELK error.
C-
 805  CONTINUE
      CALL ERRMSG('RZDELK failed', 'DUPLICATE_EVENT_FILTER',
     &  'Error deleting key', 'F')
      GO TO 999
C-
C- RZFILE error.
C-
 806  CONTINUE
      CALL ERRMSG('RZFILE failed', 'DUPLICATE_EVENT_FILTER_INI',
     &  'Error initializing RZ file', 'F')
      GO TO 999
 999  RETURN
      END
