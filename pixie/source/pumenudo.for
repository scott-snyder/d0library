      SUBROUTINE PUMENUDO(TITLE,MENNAM,COMMAND)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : PIXIE routine to handle call to MENUDO. This
C-   routine should be used in all PIXIE user applications instead of
C-   MENUDO. This routine keeps track of the path thu the menu tree, sets
C-   the view port etc. If the flag EXIT_PIXIE is TRUE then the command
C-   returned will be 'EXIT'.
C-
C-   Inputs  : TITLE    [C*]    Menu title
C-             MENNAM   [C*]    Menu/Package name
C-   Outputs : COMMAND  [C*]    Command selected
C-   Controls: None
C-
C-   Created   6-SEP-1990   Harrison B. Prosper
C-   Updated  27-SEP-1990   Harrison B. Prosper
C-      Simplified logic
C-   Updated   3-OCT-1990   Lupe Howell  Define names of parameters
C-      and screen arrays
C-   Updated  19-OCT-1990   Harrison B. Prosper
C-      Move updating of command stack (not fifo) before call to setview
C-   Updated   8-JAN-1991   Harrison B. Prosper
C-   Updated   9-JAN-1991   Harrison B. Prosper
C-      Add calls to PUOPEN_CHECK_VIEW3D to disable checking of
C-      VIEW3D parameter within PUMENUDO.
C-   Updated  15-JAN-1991   Harrison B. Prosper
C-      Bug fix
C-   Updated   1-MAR-1991   Lupe Howell
C-      Bug Fix
C-   Updated  20-MAR-1991   Harrison B. Prosper
C-      Set RCP bank automatically.
C-   Updated  13-MAY-1991   Harrison B. Prosper
C-      Add PICK processing
C-   Updated  18-JUN-1991   Nobuaki Oshima, Harrison B. Prosper
C-      Improve handling of PICK and ROTATE
C-   Modified 11-JUL-1991   Nobuaki Oshima
C-      Does NOT call PUTEXT_CLEAR when 'PICKING' was .TRUE.
C-   Updated  30-AUG-1991   Lupe Howell  Do not do a reset if the Change
C-      Display Mode toggle is selected.
C-   Updated  27-SEP-1991   Lupe Howell, Harrison B. Prosper
C-      Sequence Display implementation
C-   Updated  21-NOV-1991   Lupe Howell, Harrison B. Prosper
C-      Setting intyerrupt menu for sequential display every time
C-   Updated  24-JAN-1992   Lupe Howell   Update for SGI
C-   Updated  16-MAY-1992   Nobuaki Oshima, Harrison B. Prosper
C-      Handling Non-existing Package in Combined Menu.
C-   Updated  30-JUN-1992   Lupe Howell   Add queue mode and modify
C-      command stack if exit is called
C-   Updated   2-SEP-1992   Lupe Howell the views will be reset if a HADRCOPY
C-   Updated  13-OCT-1992   Lupe Howell   The views will be reset if HARDCOPY
C-     and NOT Superimpose
C-   Updated  17-NOV-1992   Lupe Howell   The reset of view is done only if 
C-     HARDWARE ROTATE flag is NOT set.
C-   Updated  23-MAR-1993   Lupe Howell   Implementing Batch mode 
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      CHARACTER*(*) TITLE
      CHARACTER*(*) MENNAM
      CHARACTER*(*) COMMAND
C
      INTEGER LTITLE, LMENNAM, IER, I, J, L, LPACK
      INTEGER ISTACK, OLD_ISTACK, OVERFLOWS, SCREEN_ID
      LOGICAL FLGVAL, FLGCHK, PU_SET_RCP_BANK
      LOGICAL STACK_ERROR, PICK_MODE, VALID_SCREEN,SKIP
      LOGICAL SYSTEM_MENU_NOT_ADDED,SYSTEM_COMMAND, RESET_RCP_BANK
      LOGICAL ACTIVE, EXIT, OK, TOP_MENU, NO_SCREEN, FORCE_AN_EXIT
      LOGICAL INTAST, PACK_RCP_EXIST,QUEUE_MODE,PU_POP_QUEUE
      CHARACTER*32 ACTIVE_PACKAGE, OLD_ACTIVE_PACKAGE
      CHARACTER*32 RCP_BANK, OLD_RCP_BANK
      CHARACTER*80 MESS
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:PXCOMK.INC'
C----------------------------------------------------------------------
      LOGICAL FIRST
      SAVE FIRST,ISTACK,RCP_BANK,ACTIVE_PACKAGE
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      CALL PUOPEN_CHECK_VIEW3D(.FALSE.)
      SKIP = .FALSE.
C
C ****  Check for any open segments and close them
C
C      call JFILES(2,1,25)
C      call JSETDB(2)
      IF ( FLGVAL('DI3_INI') ) THEN
        CALL PX_CLOSE_OPEN_SEGMENT
      ENDIF
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
C
C ****  Define names of Parameter and Screen arrays
C ****  Check for EZPICK/EZRSET errors.  If an RCP file was
C ****  left without a reset.
C
        PARNAME  = 'PXPARAMS'
        SCRENAME = 'PXSCREEN'
        RESET_RCP_BANK = .FALSE.
      ENDIF
C
C ****  Reset RCP bank to previously selected one
C
      IF ( RESET_RCP_BANK ) THEN
        RESET_RCP_BANK = .FALSE.        ! IMPORTANT
C
        OLD_RCP_BANK   = RCP_BANK
        OLD_ISTACK     = ISTACK
        CALL EZTELL(RCP_BANK,L)
        CALL EZPICK_GET_PTR(ISTACK,OVERFLOWS)
C
        STACK_ERROR = .NOT. ((ISTACK   .EQ. OLD_ISTACK) .AND.
     &                       (RCP_BANK .EQ. OLD_RCP_BANK))
        IF ( STACK_ERROR ) THEN
          MESS = ' PUMENUDO: *** EZPICK...EZRSET stack ERROR; bank: '//
     &           RCP_BANK
          CALL STAMSG(MESS,.TRUE.)
          CALL PU_RESET_RCP_BANK
        ENDIF
        CALL PU_RESET_RCP_BANK
      ENDIF
C
C ****  Check EXIT flag
C
      IF ( FLGVAL('EXIT_PIXIE') ) THEN
        COMMAND = 'EXIT'
        GOTO 999
      ENDIF
C
C ****  If the NEXT_EVENT flag is set then re-draw the display title
C ****  and set the NEXT_EVENT flag to FALSE, unless a specific event
C ****  has been asked for. In that case exit if the current event
C ****  is NOT the one requested.
C
      IF ( FLGVAL('NEXT_EVENT') ) THEN
C
C ****  If a specific event has been asked for then exit
C ****  if the current event is not yet the correct one
C
        IF ( EVTCNT .GT. 0 ) THEN
          CALL PUFINDEVNT(EXIT)
          IF ( EXIT ) THEN
            COMMAND = 'EXIT'
            GOTO 999
          ENDIF
        ENDIF
C
C ****  If NEXT EVENT Check on the sequence counters the skip events and
C ****  event_count.   If the requested number of skips is not reach exit
C ****  and get next event
C
        IF ( FLGVAL('SEQUENCE_MODE') ) THEN
C
C ****  If for some reason the interrupt menu is not active
C ****  reactivate it.  In PXMAIN the interrupt menu is
C ****  deactivated when it exit
C
          IF ( .NOT. INTAST() ) THEN
            CALL PX_SETUP_SEQUENCE_INTERRUPT
          ENDIF
          CALL PX_CHECK_SEQUENCE(EXIT)
          IF( EXIT ) THEN
            COMMAND = 'EXIT'
            GOTO 999
          ENDIF
        ENDIF
C
C ****  After reaching the correct event clear the command stack and
C ****  set the NEXT_EVENT flag to false
C
        CALL PX_ZERO_COMMAND_STACK
        CALL FLGSET('NEXT_EVENT',.FALSE.)
C
C ****  Draw the Event Display heading only if NOT if BATCH mode
C
        IF ( .NOT. (FLGVAL('BATCH') ) )
     &     CALL PUHEAD('D0 Event Display')
      ENDIF
C
C ******************************************
C ****  Pick appropriate RCP bank
C ******************************************
C
      CALL WORD(MENNAM(1:LEN(MENNAM)),I,J,LMENNAM)
      TOP_MENU = MENNAM(I:J) .EQ. 'PIXIE'
C
C ****  Get the name of the active package
C ****  If at the top menu active package is PIXIE
C
      IF ( TOP_MENU ) THEN
        ACTIVE_PACKAGE = MENNAM(I:J)
        LPACK = LMENNAM
      ELSE
C
C ****  This NOT the top menu; a package has been selected
C
        CALL PU_ACTIVE_PACKAGE(ACTIVE_PACKAGE)
        CALL WORD(ACTIVE_PACKAGE,I,J,LPACK)
C
C **** Setting the RCP file of the selected package
C
        PACK_RCP_EXIST = PU_SET_RCP_BANK(ACTIVE_PACKAGE)
        IF ( PACK_RCP_EXIST ) THEN
          RESET_RCP_BANK = .TRUE.               ! Reset bank upon NEXT entry.
          CALL EZTELL(RCP_BANK,L)               ! Get RCP-bank name
          CALL EZPICK_GET_PTR(ISTACK,OVERFLOWS) ! Get RCP stack pointer
C
C ****  If this is a new menu add the system menu items to it
C
          SYSTEM_MENU_NOT_ADDED = .NOT. FLGCHK(MENNAM(1:LMENNAM))
          IF ( SYSTEM_MENU_NOT_ADDED ) THEN
            CALL SYSMENU(MENNAM(1:LMENNAM))
            CALL FLGBK(MENNAM(1:LMENNAM),1)
          ENDIF
        ENDIF
      ENDIF
C
C ****  Get next command
C
      VALID_SCREEN = .FALSE.
      LTITLE = LEN(TITLE)
      ACTIVE = .TRUE.
      DO WHILE ( ACTIVE )
C
C ****  Get command either from command COMMAND_FIFO, SEQUENCE_FIFO
C ****  or from MENUDO.
C ****  Note: PU_GET_NEXT_COMMAND will set the flag USE_COMMAND_FIFO
C ****  to FALSE when the last command in the fifo is returned.
C ****  PU_GET_NEXT_SEQUENCE will set the flag USE_SEQUENC_FIFO to FALSE
C ****  when the number of displayed sequence match the number requested
C
        IF ( FLGVAL('USE_COMMAND_FIFO') ) THEN
          CALL PX_GET_NEXT_COMMAND(COMMAND)
        ELSE
C
C ****  Check whether we are in PICK or ROTATING mode
C ****  Set the command to it if it is
C
          IF     ( FLGVAL('PICKING') ) THEN
            COMMAND = 'PICK'
          ELSEIF ( FLGVAL('ROTATING') ) THEN
            COMMAND = 'ROTATE'
          ELSE
C
C ****  If the SEQUENCE flag is on get the command from the 
C ****  SEQUENCE_COMMAND queue 
C
            IF ( FLGVAL('USE_SEQUENC_FIFO') ) THEN
              CALL PX_GET_NEXT_SEQUENCE(COMMAND)
            ELSE
C
C ****  Else get command from check the global queue.  
C
              QUEUE_MODE = PU_POP_QUEUE(COMMAND) 
C
C ****  If the queue is empty get command from user
C
              IF  ( .NOT. QUEUE_MODE ) THEN
                CALL MENUDO(TITLE(1:LTITLE),MENNAM(1:LMENNAM),COMMAND)
              ENDIF
            ENDIF
          ENDIF
C
          CALL STAMSG
     &        ('                                               ',.TRUE.)
C
C ****  SKIP is use to skip the set of views.  
C ****  If in BATCH MODE and HARDCOPY requested set DI3000 
C ****  and SKIP to FALSE
C
          IF( ( FLGVAL('BATCH') ) .AND. 
     &      ( COMMAND .EQ. 'HARDCOPY') ) THEN
            CALL PU_SET_HRDCP_DEVICE
            SKIP = .FALSE.
C
C ****  Set SKIP if in BATCH mode and NOT TOP_MENU
C
          ELSEIF( ( FLGVAL('BATCH') ) .AND. ( .NOT. TOP_MENU ) ) THEN
            SKIP = .TRUE.
          ENDIF
C
C **** If a the command is not one of the following utilities
C **** Check if the Hardcopy flag was active to reset the device,
C **** Reset the parameters (viewing/param) of the previeous view if
C **** they were set.
C
          IF ( ( COMMAND .NE. 'ZOOM A VIEW' ) .AND.
     &         ( COMMAND .NE. 'HARDCOPY'    ) .AND.
     &         (.NOT.FLGVAL('SUPERIMPOSE')  ) .AND.
     &         ( COMMAND .NE. 'PICK'        ) .AND.
     &         ( COMMAND .NE. 'SUPERIMPOSE' ) .AND.
     &         ( COMMAND .NE. 'ROTATE'      ) .AND.
     &         ( COMMAND .NE. 'CHANGE DISPLAY MODE' ) ) THEN
C
C ****  If HARDCOPY flag was active reset the device to the
C ****  terminal screen.
C
            IF ( FLGVAL('HARDCOPY') ) THEN
              CALL PURESET_DEVICE
            ENDIF
C
C ****  Check whether or not to reset the SCREEN parameters.
C ****  Should only be done if the current command is a new
C ****  command entered via the menu and the 'HARDWARE ROTATE' 
C ****  flag is NOT set.   Also set the combined mode flag to false.
C
            IF ( .NOT. FLGVAL('HARDWARE_ROTATE') ) THEN
              CALL PU_RESET_VIEWS
            ENDIF
            CALL FLGSET('COMBINED_MODE',.FALSE.)
          ENDIF
C
C ****  Clear PICK flag
C
          CALL PU_CLEAR_PICK
        ENDIF
C
C ****  Check if current command is a system command.
C ****  If the system has issued an EXIT command then
C ****  return from PUMENUDO.
C
        CALL PUSYSTEM(COMMAND,SYSTEM_COMMAND)

        IF     ( SYSTEM_COMMAND ) THEN
C
          IF ( COMMAND(1:4) .EQ. 'EXIT' ) THEN
            ACTIVE = .FALSE.
          ENDIF
C
C ****  Check for a user EXIT command.
C ****  If we are at the top-most menu (PIXIE) or there
C ****  is only one PIXIE package active AND we are at the top-most
C ****  menu level of that package shut-down PIXIE and force an exit
C ****  from PIXIE.  If we are NOT in COMBINE mode decrease the command
C ****  stack as be backup up in the menu levels
C
        ELSEIF ( COMMAND(1:4) .EQ. 'EXIT' ) THEN
          ACTIVE = .FALSE.
          FORCE_AN_EXIT = TOP_MENU .OR.
     &                   ((NPACKAGE.EQ.1) .AND.
     &                   (ACTIVE_PACKAGE(1:LPACK) .EQ.
     &                   MENNAM(1:LPACK)))
          IF ( FORCE_AN_EXIT ) THEN
            CALL PUEXIT                           ! Shut-down PIXIE
            CALL FLGSET('EXIT_PIXIE',.TRUE.)      ! Force an EXIT from PIXIE
          ENDIF
          IF ( .NOT. FLGVAL('COMBINED_MODE') ) THEN
            CALL PU_UPDATE_COMMAND_STACK(ACTIVE_PACKAGE,COMMAND)
          ENDIF
C
        ELSE
C
C ****  The command is neither a system command nor an EXIT command.
C ****  If the command is either:
C ****
C ****          1) issued from the top-most menu OR
C ****          2) a combined-view command OR
C ****          3) a single-view command THEN return
C ****
C ****  Update the command stack with the current command
C ****  at the current menu level, but only if we are not
C ****  yet in combined mode.
C
          IF ( .NOT. FLGVAL('COMBINED_MODE') ) THEN
            CALL PU_UPDATE_COMMAND_STACK(ACTIVE_PACKAGE,COMMAND)
          ENDIF
C
C ****  Associate the PACKAGE/COMMAND with a unique ID
C
          CALL PU_GET_SCREEN_ID(ACTIVE_PACKAGE,COMMAND,SCREEN_ID)
C
C ****  Check if current command is a one which is
C ****  NOT associated with a screen.
C
          CALL PU_CHECKMENU(COMMAND,NO_SCREEN)
C
          IF ( TOP_MENU .OR. NO_SCREEN ) THEN
            ACTIVE = .FALSE.
            VALID_SCREEN = .FALSE.
          ELSE
C
C ****  This is a single or combined view command; If this is a single
C ****  view command then find the corresponding screen parameters
C ****  and set the view accordingly. If this is a combined view
C ****  command then go into combined view mode.
C
            IF ( PACK_RCP_EXIST .AND.  (.NOT. SKIP)  ) THEN
              CALL SETVIEW(ACTIVE_PACKAGE,COMMAND,IER)
            ELSE
              COMMAND = ' '
              IER = 0
            ENDIF
            IF     ( IER .EQ. 0 ) THEN
              ACTIVE = .FALSE.
              VALID_SCREEN = .TRUE.
C
C ****  Perform all screen-dependent clears here for any facility
C ****  which needs it.
C
              IF ( .NOT. FLGVAL('PICKING') ) THEN
                CALL PUTEXT_CLEAR(SCREEN_ID)
              ENDIF
            ELSE
C
C ****  IER = 1 indicates that we are in combined mode
C
              IF ( IER .NE. 1 ) THEN
                MESS =' No screen was found for the command: '//
     &              COMMAND
                CALL ERRMSG('NOSCREEN','PUMENUDO',MESS,'S')
              ENDIF
            ENDIF
          ENDIF
        ENDIF
   20   CONTINUE
      ENDDO
C
C ****  Pass the screen ID to any facility which needs it
C
      IF ( VALID_SCREEN .AND. (.NOT. SKIP ) ) THEN
        CALL PUTEXT_SCREEN(SCREEN_ID)
      ENDIF
C
  999 CONTINUE
      CALL PUOPEN_CHECK_VIEW3D(.TRUE.)
      RETURN
      END
