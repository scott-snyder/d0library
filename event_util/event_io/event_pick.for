      FUNCTION EVENT_PICK()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      call PICK_EVENTS to find if event is on list
C-
C-   Created   9-JUN-1992   Serban Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL EVENT_PICK
      LOGICAL WRITE_ONE,PICK_EVENTS,FLGCHK
      CHARACTER*16 EXT
      INTEGER IER,LENF
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      IF(FIRST) THEN
        WRITE_ONE=.FALSE.
        EXT='.DAT'
        CALL INRCP('PICK_EVENTS_RCP',IER)
        IF(IER.NE.0) CALL ERRMSG('No PICK_EVENTS.RCP file',
     &    'EVENT_PICK', ' ','W')
        CALL EZPICK('PICK_EVENTS_RCP')
        CALL EZGET('WRITE_ONE_EVENT',WRITE_ONE,IER)
        CALL EZGETS('FILE_EXTENSION',1,EXT,LENF,IER)
        CALL EVWRIT_FILE_EXT(EXT)
        IF(.NOT.FLGCHK('WRITE_EVENT')) CALL FLGBK('WRITE_EVENT',1)
        FIRST=.FALSE.
      ENDIF
C
      EVENT_PICK=PICK_EVENTS()
      IF(EVENT_PICK.AND.WRITE_ONE) THEN
        CALL FLSETS('WRITE_STREAM_',.FALSE.)  ! off regular output streams
        CALL FLGSET('WRITE_EVENT',.TRUE.)     ! on writing one event file
        CALL EVWRIT
      ENDIF
  999 RETURN
      END
