      FUNCTION PICK_EVENTS_HEAD()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      strip events after reading header record
C-      calls PICK_EVENTS if USER_HEADER is true, otherwise do nothing
C-
C-    ENTRY PICK_EVENTS_INI turn on reading event record header
C-
C-   Created   2-JUN-1992   Serban Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL PICK_EVENTS_HEAD,PICK_EVENTS,PICK_EVENTS_INI
      LOGICAL USE_HEADER,FIRST
      INTEGER RUNNO
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF(FIRST) THEN
        FIRST=.FALSE.
        USE_HEADER=.TRUE.
        IF(RUNNO().LE.0) USE_HEADER=.FALSE.
        CALL EVTIN_HEADER(USE_HEADER)
      ENDIF
      PICK_EVENTS_HEAD=.TRUE.
      IF ( USE_HEADER ) THEN
        CALL PICK_DONE(.FALSE.)
        PICK_EVENTS_HEAD=PICK_EVENTS()
        CALL PICK_DONE(.TRUE.)
      ENDIF
C
      GOTO 999
C
C
      ENTRY PICK_EVENTS_INI()
      CALL EVTIN_HEADER(.TRUE.)   ! activate reading header record
      PICK_EVENTS_INI=.TRUE.
  999 RETURN
      END
