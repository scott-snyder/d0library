      SUBROUTINE NOI_EVENT_SELECT(EVNO)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : LOADS THE CORRECT LHEAD AND LHEADR INFO
C-   INTO ZEBCOM FOR EVENT NUMBER GIVEN BY EVNO
C-
C-   Inputs  : EVNO = EVENT NUMBER
C-   Outputs :
C-   Controls:
C-
C-   Created   8-AUG-1991   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER EVNO
      INCLUDE 'D0$INC:EVENT_HEAD_LINKS.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
C----------------------------------------------------------------------
      IF ( EVNO.GT.NEV_MAX ) THEN
        CALL ERRMSG('NOISY','NOI_EVENT_SELECT',
     &    ' EVENT NUMBER EXCEEDS MAXIMUM ','W')
        RETURN
      ENDIF
      LHEAD = LEVENT_HEAD(EVNO)
      IF(EVNO.EQ.1)THEN
        LHEADR = LRUN_HEAD(1)
      ELSE
        LHEADR = LRUN_HEAD(2)
      ENDIF
  999 RETURN
      END
