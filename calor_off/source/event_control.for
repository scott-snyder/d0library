      SUBROUTINE EVENT_CONTROL (NEVENT,COMMAND,CONTROL,NCONT,PROCESS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Controls skipping/processing events. The
C-      available commands are 'SKIP', 'PROC' followed by a set of
C-      integers. If event NEVENT is to be processed then the flag
C-      PROCESS = TRUE. Use the logical function TRIGGER_BIT_CHECK
C-      to select events with particular triggers.
C-
C-   Inputs  : NEVENT           [I]     Event counter number
C-             COMMAND          [C*]    Command
C-             CONTROL(*)       [I]     Command values
C-             NCONT            [I]     Number of command values
C-   Outputs : PROCESS          [L]     TRUE => process event
C-
C-   Controls:
C-
C-   Created  31-JAN-1990   Harrison B. Prosper
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NEVENT
      CHARACTER*(*) COMMAND
      INTEGER CONTROL(*)
      INTEGER NCONT,EVONUM,RUNNO
      LOGICAL PROCESS,TRIGGER_BIT_CHECK
C
      INTEGER I,J,K,L
      INTEGER JEVENT,JPOINT,RUN,EVENT
      LOGICAL SKIP, EVENT_SWITCH, FLIP
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      SAVE JEVENT,JPOINT,SKIP,EVENT_SWITCH
C----------------------------------------------------------------------
C
C ****  Check for first event
C
      IF ( NEVENT .EQ. 1 ) THEN
        L = LEN(COMMAND)
        CALL UPCASE (COMMAND(1:L),COMMAND(1:L))
        SKIP = INDEX(COMMAND(1:L),'SKIP') .GT. 0
        EVENT_SWITCH = INDEX(COMMAND(1:L),'EV') .GT. 0
C
C ****  If command is SKIP then PROCESS is set FALSE, initially
C
        PROCESS= .NOT. SKIP             ! Setup "square-wave" control
        JPOINT = 1                      ! First command value
        JEVENT = 0
      ENDIF
C
C ****  Check command
C
      IF ( EVENT_SWITCH ) THEN
C
        IF ( (JPOINT+1) .LE. NCONT ) THEN
C
C ****  SKIP/EVENT or PROCESS/EVENT command
C ****  Get Run and Event number
C
          RUN   = RUNNO()
          EVENT = EVONUM()
          IF ( (RUN   .EQ. CONTROL(JPOINT)) .AND.
     &         (EVENT .EQ. CONTROL(JPOINT+1)) ) THEN
            JPOINT = JPOINT + 2         ! Go to next (RUN,EVENT) pair
C
            IF ( SKIP ) THEN
              PROCESS = .FALSE.       ! Skip THIS (run,event)
            ELSE
              PROCESS = .TRUE.       ! Process THIS (run,event)
            ENDIF
          ELSE
            IF ( SKIP ) THEN
              PROCESS = .TRUE.        ! Process ALL other events
            ELSE
              PROCESS = .FALSE.       ! Skip ALL other events
            ENDIF
          ENDIF
C
        ELSEIF ( JPOINT .EQ. (NCONT+1) ) THEN
          JPOINT = JPOINT + 1           ! Come here once only.
          PROCESS = .NOT. PROCESS
        ENDIF
C
      ELSE
C
C ****  SKIP or PROCESS command; set up "square-wave" control
C
        IF ( JPOINT .LE. NCONT ) THEN
          IF ( (NEVENT-JEVENT) .GT. CONTROL(JPOINT) ) THEN
            JEVENT = JEVENT + CONTROL(JPOINT)
            JPOINT = JPOINT + 1
            PROCESS = .NOT. PROCESS
          ENDIF
        ENDIF
      ENDIF
  999 RETURN
      END
