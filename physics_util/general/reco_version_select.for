      LOGICAL FUNCTION RECO_VERSION_SELECT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Apply reco version range cut.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Entry points:  RECO_VERSION_SELECT_INI - Initialization.
C-                  RECO_VERSION_SELECT_END - Statistical summary.
C-
C-   Created  10-Dec-1994   Herbert Greenlee
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL RECO_VERSION_SELECT_INI, RECO_VERSION_SELECT_END
      INCLUDE 'D0$INC:ZEBCOM.INC'
C-
C- Variables from RECO_VERSION_SELECT_RCP.
C-
      LOGICAL DO_RECO_VERSION_SELECT
      REAL MIN_RECO_VERSION, MAX_RECO_VERSION
      LOGICAL ALLOW_VERSION_ZERO
C-
C- Statistics
C-
      INTEGER NUM_EVENT_INPUT, NUM_EVENT_OUTPUT
C-
C- Other variables and functions.
C-
      INTEGER IER
      INTEGER VERSION, PASS
      REAL FLOAT_VERSION
      CHARACTER*80 MSG
      INTEGER LUN, SSUNIT
      LOGICAL FIRST
C-
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      RECO_VERSION_SELECT = .TRUE.
      NUM_EVENT_INPUT = NUM_EVENT_INPUT + 1
C-
C- Test reco version
C-
      IF(DO_RECO_VERSION_SELECT)THEN
        CALL RECO_VERSION(VERSION, PASS)
        FLOAT_VERSION = VERSION + 0.01*PASS
        IF(
     &     (FLOAT_VERSION .GT. 0. .AND. 
     &      FLOAT_VERSION .LT. MIN_RECO_VERSION - 0.001)
     &       .OR.
     &     (FLOAT_VERSION .GT. MAX_RECO_VERSION + 0.001)
     &       .OR.
     &     (FLOAT_VERSION.EQ.0. .AND. .NOT.ALLOW_VERSION_ZERO)
     &    )THEN
          WRITE(MSG,100)FLOAT_VERSION
 100      FORMAT('Bad RECO version',F6.2,' -- event skipped')
          CALL ERRMSG('Bad RECO version', 'RECO_VERSION_SELECT',
     &      MSG, 'W')
          RECO_VERSION_SELECT = .FALSE.
        ENDIF
      ENDIF
C-
C- Event statistics
C-
      IF(RECO_VERSION_SELECT)NUM_EVENT_OUTPUT = NUM_EVENT_OUTPUT + 1
      GO TO 999
 
      ENTRY RECO_VERSION_SELECT_INI()
C-
C- Initialization entry point
C-
      RECO_VERSION_SELECT_INI = .TRUE.
      IF(FIRST) THEN
C-
C- Zero statistics
C-
        NUM_EVENT_INPUT = 0
        NUM_EVENT_OUTPUT = 0
C-
C- Read RCP parameters.  First read from RECO_VERSION_SELECT_RCP.
C-
        CALL EZPICK_NOMSG('RECO_VERSION_SELECT_RCP', IER)
        IF(IER.NE.0)THEN
          CALL INRCP('RECO_VERSION_SELECT_RCP', IER)
          CALL EZPICK_NOMSG('RECO_VERSION_SELECT_RCP', IER)
        ENDIF
        IF(IER.EQ.0)
     &    CALL EZGET('DO_RECO_VERSION_SELECT',DO_RECO_VERSION_SELECT,
     &    IER)
        IF(IER.EQ.0. .AND. DO_RECO_VERSION_SELECT) THEN
          IF(IER.EQ.0)CALL EZGET('MIN_RECO_VERSION',MIN_RECO_VERSION,
     &      IER)
          IF(IER.EQ.0)CALL EZGET('MAX_RECO_VERSION',MAX_RECO_VERSION,
     &      IER)
          IF(IER.EQ.0) CALL EZGET('ALLOW_VERSION_ZERO',
     &      ALLOW_VERSION_ZERO, IER)
        ENDIF
        CALL EZRSET
        IF (IER.NE.0) CALL ERRMSG('Error in RECO_VERSION_SELECT_RCP',
     &    'RECO_VERSION_SELECT',' ','F')
        FIRST=.FALSE.
      ENDIF
      GO TO 999
 
      ENTRY RECO_VERSION_SELECT_END()
C-
C- Job summary entry point
C-
      LUN = SSUNIT()
      PRINT 500, NUM_EVENT_INPUT, NUM_EVENT_OUTPUT
      WRITE(LUN,500)NUM_EVENT_INPUT, NUM_EVENT_OUTPUT
 500  FORMAT(/' RECO_VERSION_SELECT package statistics'/
     &  /1X,I8,' Events processed'
     &  /1X,I8,' Events selected'/)
 999  RETURN
      END
