      FUNCTION PICK_EVENTS()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      find whether event is on list EVENT_LIST
C-
C-   Created   9-JUN-1992   Serban Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL PICK_EVENTS,PICK_DONE
      LOGICAL EV_CHECK_LIST,DONE,DID_IT
      INTEGER RUN,ID
      DATA DID_IT/.FALSE./
C----------------------------------------------------------------------
      PICK_EVENTS=.TRUE.
      IF ( .NOT.DID_IT ) THEN
        CALL EVNTID(RUN,ID)
        PICK_EVENTS=EV_CHECK_LIST(RUN,ID)
      ENDIF
      GOTO 999
C
C
      ENTRY PICK_DONE(DONE)
      DID_IT=DONE
  999 RETURN
      END
