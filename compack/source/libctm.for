      SUBROUTINE LIBCTM
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Cancel MENUDO timer if set.
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created   9-AUG-1990   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C&IF VAXVMS
      INTEGER STATUS
      INTEGER SYS$CANTIM
      INCLUDE 'D0$INC:KEYCOM.INC'
C----------------------------------------------------------------------
      STATUS = SYS$CANTIM(%VAL(TIMER_ID),)
      IF ( .NOT. STATUS ) CALL MSGSCR(STATUS,' ')
      TIMER_ON = .FALSE.
C&ENDIF
  999 RETURN
      END
