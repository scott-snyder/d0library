      SUBROUTINE PUSYSTEM (COMMAND,SYSTEM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Perform PIXIE system command.
C-   The flag EXIT_PIXIE is set TRUE if exit from PIXIE is
C-   required. This routine contains the entry points:
C-
C-      PU_GET_ACTIVE_COMMAND(COMM) and PU_GET_ACTIVE_SCREEN(COMM).
C-
C-   For a single-view command the strings returned by these routines
C-   are identical. If the command is a multi-view command then the
C-   first routine will return the current multi-view command while
C-   PU_GET_ACTIVE_SCREEN will return the current view (screen) of the
C-   multi-view command.
C-
C-   Inputs  : COMMAND  [C*]    System or screen command
C-
C-   Outputs : COMMAND  [C*]    System or screen command
C-             SYSTEM   [L]     TRUE if a system command
C-
C-   Created  11-JUL-1990   Lupe Howell
C-   Updated   7-SEP-1990   Harrison B. Prosper
C-      Changed argument
C-   Updated  26-SEP-1990   Harrison B. Prosper
C-   Updated   8-OCT-1990   Lupe Howell  The Zoom utility uses the FIFO command
C-      from the command stack
C-   Updated  15-NOV-1990   Harrison B. Prosper
C-      Add Set Path and New Display
C-   Updated  28-NOV-1990   Harrison B. Prosper
C-      Add Superimpose mode
C-   Updated  17-DEC-1990   Harrison B. Prosper
C-      Add entry point PU_GET_ACTIVE_SCREEN
C-   Updated  20-FEB-1991   Harrison B. Prosper
C-      Add Other Options menu
C-   Updated  13-MAY-1991   Harrison B. Prosper
C-      Add PICK
C-   Updated  28-MAY-1991   Harrison B. Prosper
C-      Add ROTATE
C-   Updated  25-SEP-1991   Lupe Howell, Harrison, B. Prosper
C-      Start Sequence mode added
C-   Updated   1-OCT-1991   Lupe Howell
C-     Hardcopy improved so it will not redrawn the last view if no hardcopy
C-     driver set.
C-   Updated  10-OCT-1991   Lupe Howell  The last command chosen was send as
C-     a parameter in the call to DISPLAYMOD
C-   Updated   5-JUN-1992   Nobuaki Oshima - Add 'Dump Bank' menu.
C-   Updated  25-SEP-1992   Nobuaki Oshima - Add 'GOTO EVENT' flag.
C-   Updated  14-DEC-1992   Vipin Bhatnagar
C-      Add 'MODIFY SCAN' and 'WRITE EVENT' for PIXIE Frame.
C-   Updated  22-DEC-1992   Vipin Bhatnagar - Add Routine 'PSAVE_WRITE'
C-   Updated  16-JUL-1993   Vipin Bhatnagar - Add call to 'PSAVE_WRITE'
C-      when exiting from pixie after scanning an event
C-   Updated   4-OCT-1993   Lupe Howell - Harcopy option modified to reset 
C-       device when harcopys are requested consecutavily
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) COMMAND
      CHARACTER*(*) COMM
      LOGICAL SYSTEM
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:PXCOMK.INC'
C----------------------------------------------------------------------
      LOGICAL LUPE,TEMP
      LOGICAL FLGVAL
      INTEGER I,J,K,IDX,IER
      REAL    PAUSE_TIME,VALUE
      CHARACTER*40 OLD_COMMAND, OLD_SCREEN
      SAVE OLD_COMMAND, OLD_SCREEN
      DATA OLD_COMMAND  /' '/
      DATA OLD_SCREEN   /' '/
C----------------------------------------------------------------------
C
C ****  Next event
C
      SYSTEM = .TRUE.
C
      IF    ( COMMAND .EQ. 'NEXT EVENT' ) THEN
C
        CALL PSAVE_WRITE
C
        CALL FLGSET('EXIT_PIXIE',.TRUE.)
        CALL FLGSET('NEXT_EVENT',.TRUE.)
        CALL FLGSET('USE_COMMAND_FIFO',.TRUE.)
        COMMAND = 'EXIT'
C
C ****  Go to Event
C
      ELSEIF( COMMAND .EQ. 'GO TO EVENT' ) THEN
C
        CALL PSAVE_WRITE
C
        CALL PU_GET_EVNT_NUM
        CALL FLGSET('EXIT_PIXIE',.TRUE.)
        CALL FLGSET('NEXT_EVENT',.TRUE.)
        CALL FLGSET('GOTO_EVENT',.TRUE.)
        CALL FLGSET('USE_COMMAND_FIFO',.TRUE.)
        COMMAND = 'EXIT'
C
C ****  Display Mode
C
      ELSEIF( COMMAND .EQ. 'CHANGE DISPLAY MODE' ) THEN
C
        LUPE = FLGVAL('AUTO_DISPLAY')
        LUPE = .NOT. LUPE
        CALL FLGSET('AUTO_DISPLAY',LUPE)
        IF ( LUPE ) THEN
          CALL STAMSG(' Mode: AUTO-Display of Same View',.TRUE.)
        ELSE
          CALL STAMSG(' Mode: MANUAL-Display of Views',.TRUE.)
        ENDIF
C
C ****  Superimpose Mode
C
      ELSEIF( COMMAND .EQ. 'SUPERIMPOSE' ) THEN
C
        LUPE = FLGVAL('SUPERIMPOSE')
        LUPE = .NOT. LUPE
        CALL FLGSET('SUPERIMPOSE',LUPE)
        IF ( LUPE ) THEN
          CALL STAMSG(' SUPERIMPOSE - ON',.TRUE.)
        ELSE
          CALL STAMSG(' Superimpose - OFF',.TRUE.)
        ENDIF
C
C ****  ZOOM
C
      ELSEIF( COMMAND .EQ. 'ZOOM A VIEW') THEN
C
        COMMAND = OLD_COMMAND
        CALL PX_ZOOM(COMMAND)
        SYSTEM  = .FALSE.
C
C ****  ROTATE
C
      ELSEIF( COMMAND .EQ. 'ROTATE') THEN
C
        COMMAND = OLD_COMMAND
        CALL PX_ROTATE(COMMAND)
        SYSTEM  = COMMAND(1:1) .EQ. ' '
C
C ****  PICK
C
      ELSEIF( COMMAND .EQ. 'PICK' ) THEN
C
        COMMAND = OLD_COMMAND
        CALL PX_PICK(COMMAND)
        SYSTEM  = COMMAND(1:1) .EQ. ' '
C
C ****  MODIFY Parameters
C
      ELSEIF( COMMAND .EQ. 'MODIFY PARAMETERS' ) THEN
C
        CALL DISPLAYMOD(OLD_COMMAND)
C
C ****  HARDCOPY
C ****  Reset the Hardcopy device if IDEV_DEV .EQ. PRINT_DEV(2), this
C ****  means that a hardcopy was called twice in the row
C
      ELSEIF( COMMAND .EQ. 'HARDCOPY' ) THEN
        IF ( FLGVAL('HARDCOPY') ) THEN
          CALL PURESET_DEVICE
        ENDIF

        CALL PU_HARDCOPY(IER)
        IF ( IER .EQ. 0 ) THEN
          COMMAND = OLD_COMMAND
          SYSTEM  = .FALSE.
        ENDIF
C
C ****  Set Path
C
      ELSEIF( COMMAND .EQ. 'SET PATH' )         THEN
C
        CALL PX_SET_PATH
C
C ****  Dump ZEBRA Banks
C
      ELSEIF( COMMAND .EQ. 'DUMP BANK' )         THEN
C
        CALL DBANK
C
C **** Modify Scan
C
      ELSEIF( COMMAND .EQ. 'MODIFY SCAN' ) THEN
C
        CALL SCAN_DO
C
C ****  START SEQUENCE Display
C
      ELSEIF( COMMAND .EQ. 'START SEQUENTIAL DISPLAY' ) THEN
        CALL PX_START_SEQUENCE
C
C ****  DELAY
C
      ELSEIF( COMMAND(1:5) .EQ. 'DELAY') THEN
        CALL PX_DELAY_SEQUENCE(COMMAND)
C
C ****  Dump Event
C
      ELSEIF( COMMAND .EQ. 'DUMP EVENT' ) THEN
C
C FLAG for CALOR_OFF
        CALL FLGSET('DUMP_THIS_EVENT',.TRUE.)
C FLAG for EXAMINE2
        CALL FLGSET('DUMP_BANKS',.TRUE.)
C FLAG for D0USER
        CALL FLGSET('DUMP_EVENT',.TRUE.)
        CALL STAMSG(' Dump Event - ON',.TRUE.)
C
C ****  Write Event
C
      ELSEIF( COMMAND .EQ. 'WRITE EVENT' ) THEN
C
C - FLAG for CALOR_OFF
        CALL FLGSET('WRITE_THIS_EVENT',.TRUE.)
C - FLAG for D0USER
        CALL FLGSET('WRITE_EVENT',.TRUE.)
C - FLAG for PIXIE
        CALL FLGSET('PX_WRITE_EVENT',.TRUE.)
        CALL STAMSG(' Write Event - ON',.TRUE.)
C
C ****  REMOVE actions to be displayed
C
      ELSEIF ( COMMAND .EQ. 'REMOVE' ) THEN
        CALL PU_MODIFY_DISPLAY_ACTIONS(OLD_COMMAND)
      ELSE
C
C ****  Not a system command
C ****  Note command only if we are not yet in combined mode
C ****  Saving scan if exiting form pixie after scanning an event
C
        IF ( COMMAND .EQ. 'EXIT' ) THEN
          CALL PSAVE_WRITE
        ENDIF
C
        IF ( .NOT. FLGVAL('COMBINED_MODE') ) THEN
          OLD_COMMAND = COMMAND
        ENDIF
        SYSTEM = .FALSE.
      ENDIF
C
C ****  Perform resets updates etc.
C
      IF ( .NOT. SYSTEM ) THEN
        OLD_SCREEN = COMMAND
      ELSE
        OLD_SCREEN = ' '
      ENDIF
  999 RETURN
C
      ENTRY PU_GET_ACTIVE_COMMAND (COMM)
      COMM = OLD_COMMAND
      RETURN
C
      ENTRY PU_GET_ACTIVE_SCREEN (COMM)
      COMM = OLD_SCREEN
      RETURN
      END
