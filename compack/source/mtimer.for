      SUBROUTINE MTIMER (COMMAND,TIMEOUT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set a time-out on the MENUDO call.
C-   Use MTIMER(' ',0) to cancel timer. Note: MTIMER(' ',0) turns
C-   off the MENUDO event flag.
C-
C-   Inputs  : COMMAND  [C*]    Command returned by MENUDO upon timeout
C-             TIMEOUT  [I]     Time-out in seconds
C-
C-   Outputs : None
C-
C-   Created   9-AUG-1990   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      CHARACTER*(*) COMMAND
      INTEGER    TIMEOUT
C&IF VAXVMS
C
      REAL    TIME_OUT
      INTEGER STATUS
      INCLUDE 'D0$INC:KEYCOM.INC'
C
      INTEGER LIB$CVTF_TO_INTERNAL_TIME
      INTEGER LIB$K_DELTA_SECONDS_F
      EXTERNAL LIB$K_DELTA_SECONDS_F
C
C----------------------------------------------------------------------
C
      IF ( TIMEOUT .GT. 0 ) THEN
        TIME_OUT = TIMEOUT
        STATUS = LIB$CVTF_TO_INTERNAL_TIME(%LOC(LIB$K_DELTA_SECONDS_F),
     &                                     TIME_OUT,
     &                                     BINARY_TIME)
        IF ( .NOT. STATUS ) CALL MSGSCR(STATUS,' ')
        TIMER_MODE = .TRUE.
        TIMER_COMMAND = COMMAND
        CALL MENUEF(.TRUE.)             ! Activate event flag
      ELSE
        IF ( TIMER_MODE ) THEN
          CALL LIBCTM                   ! Cancel timer
        ENDIF
        TIMER_MODE = .FALSE.
        IF ( COMMAND(1:1) .EQ. ' ' ) THEN
          CALL MENUEF(.FALSE.)            ! De-activate event flag
        ENDIF
      ENDIF
C&ENDIF
  999 RETURN
      END
