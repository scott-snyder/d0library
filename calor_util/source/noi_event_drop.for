      SUBROUTINE NOI_EVENT_DROP(EVNO,IFLAG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : DROPS THE EVENT GIVEN BY EVENT NUMBER
C-
C-   Inputs  : EVNO = EVENT NUMBER
C-             IFLAG =1 EVENT DROPPED
C-             IFLAG =2 RUN HEADER DROPPED
C-             IFLAG = 3 BOTH EVENT AND RUN HEADER DROPPED
C-   Outputs :
C-   Controls:
C-
C-   Created   8-AUG-1991   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER EVNO,IFLAG
      INCLUDE 'D0$INC:EVENT_HEAD_LINKS.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
C----------------------------------------------------------------------
      IF ( EVNO.GT.NEV_MAX ) THEN
        CALL ERRMSG('NOISY','NOI_EVENT_DROP',
     &    ' EVENT NUMBER EXCEEDS MAXIMUM ','W')
        RETURN
      ENDIF
      LHEAD = LEVENT_HEAD(EVNO)
      IF(EVNO.EQ.1)THEN
        LHEADR = LRUN_HEAD(1)
      ELSE
        LHEADR = LRUN_HEAD(2)
      ENDIF
      IF ( IFLAG.EQ.1.OR.IFLAG.EQ.3 ) THEN
        IF ( LHEAD.EQ.0 ) THEN
          CALL ERRMSG('NOISY','NOI_EVENT_DROP',
     &      ' HEAD LINK ZERO ','W')
        ELSE
          CALL MZDROP(IXCOM,LHEAD,' ')
          LEVENT_HEAD(EVNO) = 0
        ENDIF
      ENDIF
      IF ( IFLAG.EQ.2.OR.IFLAG.EQ.3 ) THEN
        IF ( LHEADR.EQ.0 ) THEN
          CALL ERRMSG('NOISY','NOI_EVENT_DROP',
     &      ' HEADR LINK ZERO ','W')
        ELSE
          CALL MZDROP(IXCOM,LHEADR,' ')
          LRUN_HEAD(EVNO) = 0
        ENDIF
      ENDIF
  999 RETURN
      END
