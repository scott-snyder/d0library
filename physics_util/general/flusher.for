      LOGICAL FUNCTION FLUSHER()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Flush standard output and standard error.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  10-Dec-1994   Herbert Greenlee
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C&IF VAXVMS
C&ELSE
C&      LOGICAL DO_FLUSHER
C&      INTEGER NUM_EVENT, NUM_EVENT_CYCLE
C&      INTEGER IER
C&      LOGICAL FIRST
C&C-
C&      DATA FIRST/.TRUE./
C&C----------------------------------------------------------------------
C&      IF(FIRST) THEN
C&C-
C&C- Zero event counter
C&C-
C&        NUM_EVENT = 0
C&        NUM_EVENT_CYCLE = 1
C&C-
C&C- Read RCP parameters from FLUSHER_RCP.
C&C-
C&        CALL EZPICK_NOMSG('UNIX_UTILITY_RCP', IER)
C&        IF(IER.NE.0)THEN
C&          CALL INRCP('UNIX_UTILITY_RCP', IER)
C&          CALL EZPICK_NOMSG('UNIX_UTILITY_RCP', IER)
C&        ENDIF
C&        IF(IER.EQ.0)CALL EZGET_l('DO_FLUSHER', DO_FLUSHER, IER)
C&        IF(IER.EQ.0. .AND. DO_FLUSHER) THEN
C&          IF(IER.EQ.0)CALL EZGET_i('DO_EVERY_N_EVENTS', NUM_EVENT_CYCLE, 
C&     &      IER)
C&        ENDIF
C&        CALL EZRSET
C&        IF (IER.NE.0) CALL ERRMSG('Error in UNIX_UTILITY_RCP',
C&     &    'FLUSHER',' ','F')
C&        FIRST=.FALSE.
C&      ENDIF
C&C-
C&C- End of initialization
C&C-
C&      NUM_EVENT = NUM_EVENT + 1
C&      IF(MOD(NUM_EVENT, NUM_EVENT_CYCLE).EQ.0)CALL FLUSH_STD
C&ENDIF
      FLUSHER = .TRUE.
 999  RETURN
      END
