      SUBROUTINE PU_UPDATE_COMMAND_STACK(MENU,COMMAND)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Update the command stack with the current
C-   command for the menu name given in the variable MENU.
C-
C-   Inputs  : MENU     [C*]    Menu name
C-
C-   Outputs : None
C-   Controls: None
C-
C-   ENTRY POINTS:
C-      PX_GET(SET)_COMMAND(COMMAND): to GET/SET commands from/in the
C-               command FIFO.
C-      PU_SETUP_COMMAND_FIFO: to initialize the stack pointer to one
C-               and to load the command fifo with the contents of the
C-               command stack.
C-      PX_ZERO_COMMAND_FIFO: Sets the COMMAND_FIFO pointer to 0
C-      PX_ZERO_COMAND_STACK: Sets the STACK_PTR to zero.
C-      PX_ENABLE_COMMAND_FIFO: Sets the pointer to COMMAND_FIFO (FIFO_PTR)
C-               to zero and sets the USE_COMMAND_FIFO flag.
C-      PX_DISABLE_COMMAND_FIFO: Sets the USE_COMMAND_FIFO flag to false.
C-      PX_ADD_COMMAND(COMMAND): Adds a given command to the COMMAND_FIFO queue.
C-      PX_SET_COMMAND(COMMAND): Adds to the COMMAND_FIFO the given command
C-               where FIFO_PTR points.
C-      PX_GET_COMMAND(COMMAND): Gets command from COMMAND_FIFO at the current
C-               fifo position.
C-      PX_GET_NEXT_COMMAND(COMMAND): Get the next command from COMMAND_FIFO.
C-      PX_INJECT_COMMAND(COMMAND)
C-      PX_ENABLE_SEQUENCE_FIFO: Loads the content of the COMMAND_FIFO queue
C-               into SEQUENCE_FIFO and sets environment for sequence.
C-      PX_GET_NEXT_SEQUENCE(COMMAND): Gets the next command from SEQUENCE_FIFO.
C-      PX_CHECK_SEQUENCE(EXIT): Checks the EVENT_SKIPS and EVENT_COUNT sequence
C-               counters to return the exit flag true if sequnece finished
C-      PX_STOP_SEQUENCE: Stops the sequence display mode
C-
C-   Created   8-SEP-1990   Harrison B. Prosper
C-   Updated  26-SEP-1990   Harrison B. Prosper
C-      Add COMMAND argument
C-   Updated  24-SEP-1991   Harrison B. Prosper, Lupe Howell
C-      Add PX_ENABLE_SEQUENCE_FIFO, PX_GET_NEXT_SEQUENCE,PX_CHECK_SEQUENCE,
C-      PX_STOP_SEQUENCE
C-   Updated  28-OCT-1991   Lupe Howell update to handle MAX_EVENT_SKIPS=0
C-      This will no skip any events.  Default value for MAX_EVENT_COUN
C-      changed to a large number.
C-   Updated   1-JUL-1992   Lupe Howell and Harrison B. Prosper
C-      Fixed sequence display for multiple packages and the command queue
C-      for submenu levels
C-   Updated   1-OCT-1992   Lupe Howell   
C-      PX_ZERO_COMMAND_STACK:The variable LAST_MENU was set to a blank
C-      PU_UPDATE_COMMAND_STACK: Check for out of range stack pointer
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      CHARACTER*(*) MENU
      CHARACTER*(*) COMMAND
C
      INTEGER MAXSTACK
      PARAMETER( MAXSTACK = 20 )
      CHARACTER*40 COMMAND_STACK(MAXSTACK)
C
      INTEGER MAXFIFO
      PARAMETER( MAXFIFO = 100 )
      CHARACTER*40 COMMAND_FIFO(MAXFIFO),INJECTED_COMMAND
      CHARACTER*40 SEQUENCE_FIFO(MAXFIFO),TEMP,LAST_MENU
      CHARACTER*80 STRING
C
      LOGICAL FLGVAL,INJECT_COMMAND,EXIT,UP_MENU,DOWN_MENU
      LOGICAL ACTIve,FOUND
      INTEGER I,J,K,LMENU,FIFO_DEPTH,SEQUENCE_DEPTH,SEQUENCE_PTR,SLEN
      INTEGER EVENT_SKIPS,EVENT_COUNT,MAX_EVENT_SKIPS,MAX_EVENT_COUNT
      INTEGER II,JJ,KK
      REAL    VALUE
C
      INCLUDE 'D0$INC:PXCOMK.INC'
C----------------------------------------------------------------------
      INTEGER STACK_PTR, FIFO_PTR
      DATA FIFO_PTR/0/
      DATA STACK_PTR/0/
      DATA SEQUENCE_PTR/0/
      SAVE STACK_PTR, FIFO_PTR, LAST_MENU
C
C----------------------------------------------------------------------
      CALL WORD(MENU(1:LEN(MENU)),II,JJ,KK)
      CALL WORD(COMMAND(1:LEN(COMMAND)),I,J,K)
C
C ****  Decide if we're exiting or descending to a new menu
C
      UP_MENU      = COMMAND(I:J) .EQ. 'EXIT'
      DOWN_MENU  =   ( INDEX(COMMAND_STACK(STACK_PTR),'$') ) .OR.
     &               ( MENU(II:JJ) .NE. LAST_MENU(II:JJ) )
      LAST_MENU = MENU(1:LEN(MENU))
C
C ****  Decrement the stack pointer ONLY if exiting a menu
C ****  The amount to decrement depends on the status of DONW_MENU
C ****  If DOWN_MENU decrement by one to skip menu
C ****  ELSE decrement by two to skeep the last non-menu command and the
C ****       menu from queue
C
      IF     ( UP_MENU ) THEN
        IF ( STACK_PTR .GE. 1 ) THEN
          IF( ( DOWN_MENU ) .OR. ( STACK_PTR .EQ. 1 ) ) THEN
            STACK_PTR = STACK_PTR - 1
          ELSE
            STACK_PTR = STACK_PTR - 2
          ENDIF
        ENDIF
C
      ELSEIF ( DOWN_MENU ) THEN
C
C ****  Increment stack pointer ONLY if descending to a sub-menu
C ****  One descends to a sub-menu if
C ****    1)  The last command in the stack has a $ suffix  OR
C ****    2)  The current menu differs from the last menu
C
        IF ( STACK_PTR .LT. MAXSTACK ) THEN
          STACK_PTR = STACK_PTR + 1
        ENDIF
      ENDIF
C
C ****  Update stack with current command except when is
C ****  EXIT
C
      IF ( COMMAND(1:4) .NE. 'EXIT' ) THEN
C
C ****  Check the command stack pointer for out of range 
C ****  to avoid and errors
C
        IF((STACK_PTR .LE. 0 ).OR.(STACK_PTR .GT.MAXSTACK)) THEN
          CALL OUTMSG(' COMMAND STACK POINTER OUT OF RANGE !!')
        ELSE
C
C ****  Update the command stack array
C
          COMMAND_STACK(STACK_PTR) = COMMAND
        ENDIF
      ENDIF
      RETURN
C
      ENTRY PU_SETUP_COMMAND_FIFO
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set the stack pointer to 1. Fill the
C-   command FIFO with the contents of the command stack.
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created   8-SEP-1990   Harrison B. Prosper
C-
C----------------------------------------------------------------------
C
C ****  If the USE_COMMAND_FIFO flag is set the
C ****  and the NEXT_EVENT flag transfer the commands
C ****  in the command stack to the command fifo.
C
      IF ( FLGVAL('USE_COMMAND_FIFO') ) THEN
C
        IF ( FLGVAL('NEXT_EVENT') ) THEN
          FIFO_PTR  = 0
          FIFO_DEPTH= STACK_PTR
          IF  (.NOT. FLGVAL('AUTO_DISPLAY') ) THEN
            IF ( STACK_PTR .GT. 1 ) THEN
              FIFO_DEPTH = FIFO_DEPTH - 1
            ENDIF
          ENDIF
          DO I =  1, FIFO_DEPTH
            COMMAND_FIFO(I) = COMMAND_STACK(I)
          ENDDO
        ENDIF
C
      ELSE
C
C ****  If there is only ONE package then force a descent to
C ****  the menu of that package
C
        IF ( NPACKAGE .EQ. 1 ) THEN
          FIFO_PTR = 0
          FIFO_DEPTH = 1
          CALL FLGSET('USE_COMMAND_FIFO',.TRUE.)
          COMMAND_FIFO(1) = PACKAGE(1)
        ENDIF
      ENDIF
      RETURN
C
      ENTRY PX_ZERO_COMMAND_FIFO
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set the FIFO_PTR to ZERO.
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created   8-SEP-1990   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      FIFO_PTR = 0
      RETURN
      ENTRY PX_ZERO_COMMAND_STACK
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set the STACK_PTR to ZERO.
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  30-OCT-1990   Lupe F. Howell
C-
C----------------------------------------------------------------------
      STACK_PTR = 0
      LAST_MENU  = ' '
      RETURN
C
      ENTRY PX_ENABLE_COMMAND_FIFO
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set the FIFO_PTR to zero and set the flag
C-   USE_COMMAND_FIFO to TRUE.
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created   8-SEP-1990   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      FIFO_PTR = 0
      CALL FLGSET('USE_COMMAND_FIFO',.TRUE.)
      RETURN
C
      ENTRY PX_DISABLE_COMMAND_FIFO
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set the flag USE_COMMAND_FIFO to FALSE.
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created   8-SEP-1990   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      CALL FLGSET('USE_COMMAND_FIFO',.FALSE.)
      RETURN
C
      ENTRY PX_ADD_COMMAND(COMMAND)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Add the given command to the command FIFO
C-   and increment the FIFO_PTR variable by ONE.
C-
C-   Inputs  : COMMAND   [C*]  Command
C-   Outputs : None
C-   Controls: None
C-
C-   Created   8-SEP-1990   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      FIFO_PTR   = FIFO_PTR + 1
      FIFO_DEPTH = FIFO_PTR
      COMMAND_FIFO(FIFO_PTR) = COMMAND
      RETURN
C
      ENTRY PX_SET_COMMAND(COMMAND)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Update command FIFO with given command
C-   at the current fifo pointer.
C-
C-   Inputs  : COMMAND   [C*]  Command
C-   Outputs : None
C-   Controls: None
C-
C-   Created   8-SEP-1990   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      COMMAND_FIFO(FIFO_PTR) = COMMAND
      RETURN
C
      ENTRY PX_GET_COMMAND(COMMAND)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get the command from the FIFO at the current
C-   fifo position.
C-
C-   Inputs  : COMMAND   [C*]  Command
C-   Outputs : None
C-   Controls: None
C-
C-   Created   8-SEP-1990   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      COMMAND = COMMAND_FIFO(FIFO_PTR)
      RETURN
C
      ENTRY PX_GET_NEXT_COMMAND(COMMAND)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get the NEXT command from COMMAND_FIFO
C-   If the stack pointer is equal to the fifo_depth then
C-   set the flag USE_COMMAND_FIFO to FALSE.
C-
C-   Inputs  : COMMAND   [C*]  Command
C-   Outputs : None
C-   Controls: None
C-
C-   Created   8-SEP-1990   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      FIFO_PTR = FIFO_PTR + 1
      COMMAND = COMMAND_FIFO(FIFO_PTR)
C
C ****  Check whether to inject a command at this point
C
      IF ( INJECT_COMMAND ) THEN
        COMMAND = INJECTED_COMMAND
        INJECT_COMMAND = .FALSE.
        FIFO_PTR = FIFO_PTR - 1
      ENDIF
C
      IF ( FIFO_PTR .GE. FIFO_DEPTH ) THEN
        CALL FLGSET('USE_COMMAND_FIFO',.FALSE.)
      ENDIF
      RETURN
C
      ENTRY PX_INJECT_COMMAND(COMMAND)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  : COMMAND   [C*]  Command
C-   Outputs : None
C-   Controls: None
C-
C-   Created   18-OCT-1990   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      INJECTED_COMMAND = COMMAND
      INJECT_COMMAND = .TRUE.
  999 RETURN
C
      ENTRY PX_ENABLE_SEQUENCE_FIFO
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Loads the content of COMMAND_FIFO queue into
C-   SEQUENCE_FIFO queue and activates the SEQUENCE_FIFO flag.
C-   Sets the maximum number of events skips and the maximum number of
C-   sequence display throught interpreting the action name '%SEQUENCE #1 #2'
C-   #1 is the number of events to skip between displays.  If it is set to 0
C-      it will not skip any events and it will display events sequentially
C-      If it is < 0 the default will be use, it will skip one event/display
C-   #2 Is the total number of times the event display will be executed. If it
C-      is left out or it is <= to 0 the default is 9999999, i.e. it will run
C-      until the interrupt button is use.
C-
C-   Inputs  : None
C-   Outputs : None
C-
C-   Created  24-SEP-1991   Harrison B. Prosper, Lupe Howell
C-
C----------------------------------------------------------------------
C
C ****  Copy the command_fifo queue to the sequence_fifo
C
      DO  I = 1, FIFO_DEPTH
        SEQUENCE_FIFO(I) = COMMAND_FIFO(I)
      ENDDO
C
C ****  Check for the command %SEQ followed by EXIT
C ****  and switch them in the FIFO queue
C
      I = 0
      ACTIVE = .TRUE.
      FOUND = .FALSE.
      DO WHILE ( ACTIVE )
        I = I + 1
        IF ( SEQUENCE_FIFO(I)(1:4) .EQ. '%SEQ' ) FOUND = .TRUE.
        ACTIVE = ( .NOT. FOUND ) .AND. ( I .LT. FIFO_DEPTH )
      ENDDO
      IF( FOUND )THEN
C
C ****  IF %SEQUENCE found extract it from queue and move 
C ****  the rest of the commands up in the queue
C
        STRING = SEQUENCE_FIFO(I)
        DO J = I, (FIFO_DEPTH-1)
          SEQUENCE_FIFO(J) = SEQUENCE_FIFO(J+1)
        ENDDO
C
C ****  Decreasing fifo depth
C
        FIFO_DEPTH = FIFO_DEPTH - 1  ! Deleteing the '%SEQUENCE' action
C
C ****  Get the MAX_EVENT_SKIPS and MAX_EVENT_COUNT
C
        CALL SWORDS(STRING,I,J,SLEN)
        MAX_EVENT_SKIPS = VALUE(STRING,I,J,K)
C
C ****  If no skip event parameter, the default is set to 1
C
        IF(( MAX_EVENT_SKIPS .LT. 0 ) .OR. (K .EQ. 0))THEN
          MAX_EVENT_SKIPS = 1   ! DEFAULT
        ENDIF
C
C ****  Get the sequence parameter
C
        STRING = STRING(J+1:SLEN)
        MAX_EVENT_COUNT = VALUE(STRING,I,J,K)
C
C ****  If no sequence parameter, the default is set to be
C ****  very large
C
        IF ( MAX_EVENT_COUNT .LE. 0 ) THEN
          MAX_EVENT_COUNT = 9999999 ! DEFAULT
        ENDIF
      ELSE
C
C ****  Defaults if no %SEQUENCE command found
C
        MAX_EVENT_SKIPS   = 1
        MAX_EVENT_COUNT = 9999999 ! DEFAULT
      ENDIF
C
C ****  Add NEXT EVENT to the SEQUENCE_FIFO queue
C
      IF ( MAX_EVENT_SKIPS .GE. 0 ) THEN
        SEQUENCE_FIFO(FIFO_DEPTH) = 'NEXT EVENT'
        SEQUENCE_DEPTH = FIFO_DEPTH
      ELSE
        SEQUENCE_DEPTH = FIFO_DEPTH - 1
      ENDIF
C
C ****  Initiallizing skip event and sequence counters
C
      EVENT_SKIPS = 0
      EVENT_COUNT = 0
c
C ****  Setting the FIFO flags and pointers
C
      CALL FLGSET('USE_COMMAND_FIFO',.FALSE.)
      CALL FLGSET('USE_SEQUENC_FIFO',.TRUE.)
      CALL FLGSET('SEQUENCE_MODE',.TRUE.)
      FIFO_PTR = 0
      SEQUENCE_PTR = 0
      RETURN
C
      ENTRY PX_GET_NEXT_SEQUENCE(COMMAND)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get ENTRY PX_DISABLE_COMMAND_FIFO
C-   If the stack pointer is equal to the fifo_depth then
C-   set the flag USE_COMMAND_FIFO to FALSE.
C-
C-   Inputs  : COMMAND   [C*]  Command
C-   Outputs : None
C-   Controls: None
C-
C-   Created  24-SEP-1991   Harrison B. Prosper, Lupe Howell
C-
C----------------------------------------------------------------------
C
C ****  IF in Sequence mode get command from sequence queue
C
      SEQUENCE_PTR = SEQUENCE_PTR + 1
      COMMAND = SEQUENCE_FIFO(SEQUENCE_PTR)
C
C ****  If the sequence display has been repeated the number
C ****  of CYCLES requested end sequence by disabling the
C ****  USE_SEQUENC_FIFO flag
C

      IF( SEQUENCE_PTR .GE. SEQUENCE_DEPTH ) THEN
        IF ( EVENT_COUNT .GE. MAX_EVENT_COUNT ) THEN
          CALL FLGSET('USE_SEQUENC_FIFO',.FALSE.)
        ENDIF
        SEQUENCE_PTR = 0
      ENDIF
      RETURN
C
      ENTRY PX_CHECK_SEQUENCE(EXIT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Checks the EVENT_SKIPS and EVENT_COUNT sequence
C-   counters.  The USE_SEQUENC_FIFO flag will be set to false when the
C-   requested sequence is finish. (EVENT_COUNT = MAX_EVENT-COUNT)
C-
C-   Inputs  : None
C-
C-   Outputs : EXIT [L]: Exit flag to be use in PUMENUDO to control exits
C-             .FALSE.- If number of events skipped match the maximun of events
C-                      to be skipped
C-             .TRUE. - If number of events skipped < maximum number to skip
C-
C-   Created  25-SEP-1991   Lupe Howell, Harrison B. Prosper
C-
C----------------------------------------------------------------------
      EVENT_SKIPS = EVENT_SKIPS + 1
      IF ( EVENT_SKIPS .GE. MAX_EVENT_SKIPS ) THEN
        EVENT_SKIPS = -1
        EVENT_COUNT = EVENT_COUNT + 1
        EXIT = .FALSE.
C
C ****  If this is the last sequence do not issue the Next Event command
C
        IF ( EVENT_COUNT .GE. MAX_EVENT_COUNT ) THEN
          CALL FLGSET('SEQUENCE_MODE',.FALSE.)
          SEQUENCE_DEPTH = SEQUENCE_DEPTH - 1
        ENDIF
      ELSEIF ( EVENT_SKIPS .LT. MAX_EVENT_SKIPS ) THEN
        EXIT = .TRUE.
      ENDIF
      RETURN
C
      ENTRY PX_STOP_SEQUENCE
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Stops the sequence display mode by disabling
C-   the sequence flags and decreasing the depth of SEQUENCE_FIFO queue
C-
C-   Inputs  : None
C-   Outputs : None
C-
C-   Created  26-SEP-1991   Lupe Howell, Harrison B. Prosper
C-
C----------------------------------------------------------------------
      CALL FLGSET('USE_SEQUENC_FIFO',.FALSE.)
      CALL FLGSET('SEQUENCE_MODE',.FALSE.)
      SEQUENCE_DEPTH = SEQUENCE_DEPTH - 1
      CALL CANMEN ! cancel interrupt menu
      CALL INTMSG(' Sequence will be stopped; we hope!')
      RETURN
C
      END
