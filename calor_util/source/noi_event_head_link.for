      SUBROUTINE NOI_EVENT_HEAD_LINK(EVNO,LHEAD,LHEADR,IFLAG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : STORES THE LINK OF THE EVENT HEADER
C-   SPECIFIED BY EVENT NUMBER EVNO IN PROTECTED AREA
C-
C-   Inputs  : EVNO = EVENT NUMBER
C-             LHEAD = LINK OF HEADER
C-             LHEADR = LINK OF RUN HEADER
C-             IFLAG =1 EVENT STORED
C-             IFLAG =2 RUN HEADER STORED
C-             IFLAG = 3 BOTH EVENT AND RUN HEADER STORED
C-   Outputs :
C-   Controls:
C-
C-   Created   8-AUG-1991   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER EVNO,LHEAD,LHEADR,IFLAG
      INCLUDE 'D0$INC:EVENT_HEAD_LINKS.INC'
C----------------------------------------------------------------------
      IF ( EVNO.GT.NEV_MAX ) THEN
        CALL ERRMSG('NOISY','EVENT_HEAD_LINK',
     &    ' EVENT NUMBER EXCEEDS MAXIMUM ','W')
        RETURN
      ENDIF
      IF(IFLAG.EQ.1 .OR. IFLAG.EQ.3)THEN
        LEVENT_HEAD(EVNO) = LHEAD
      ENDIF
      IF(IFLAG.EQ.2 .OR. IFLAG.EQ.3)THEN
        LRUN_HEAD(EVNO) = LHEADR
      ENDIF
  999 RETURN
      END
