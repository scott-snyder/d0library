      SUBROUTINE PX_COMBINE_VIEWS(CURRENT_PACKAGE,COMBINED_VIEW_COMMAND)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Search the RCP bank associated with the
C-   current package for the specified combined view command.
C-   If the combined view command array is present then activate and load
C-   the command fifo with the commands to execute the combined view.
C-   The last command to be executed is END% which will cause SETVIEW to
C-   reset the SCREEN and/or PARAMS values to their original values.
C-
C-   The command array
C-   should have the following format:
C-
C-      \ARRAY combined-view-command
C-              package-name    (containing the view-command(s))
C-              action-command  (associated with action routine)
C-              '%SCREEN'
C-              parameter-name  new-value
C-              parameter-name  new-value
C-                      :               :
C-              '%PARAMS'
C-              parameter-name  new-value
C-              parameter-name  new-value
C-                      :               :
C-      \END
C-
C-   Inputs  : CURRENT_PACKAGE          [C*]    Name of RCP bank containing
C-                                              the combined view command
C-             COMBINED_VIEW_COMMAND    [C*]    Name of combined view command
C-   Outputs : None
C-   Controls: None
C-
C-   Created  20-SEP-1990   Harrison B. Prosper, Lupe Howell
C-   Updated  26-FEB-1991   Lupe Howell  Allowing more than one action command
C-   Updated  22-MAR-1991   Harrison B. Prosper
C-      Remove call to EZPICK; this is done already in PUMENUDO.
C-   Updated  24-JAN-1992   Lupe Howell  Update for GSI
C-   Updated  21-OCT-1992   Lupe Howell  Display action check added
C-   Updated  27-JAN-1993   Lupe Howell  Fix bug in display action check
C-   Updated  22-MAR-1993   Lupe Howell  Add HARDCOPY to teh queue if in 
C-                             Batch mode
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) CURRENT_PACKAGE
      CHARACTER*(*) COMBINED_VIEW_COMMAND
      INCLUDE 'D0$PARAMS:PIXIE.DEF'
C----------------------------------------------------------------------
      INTEGER NUMBER_PARAMS,PTR,IER,I,J,K,L,II,JJ,KK
      INTEGER LOLD,LCOMB,LPACK,ITYPE
C
      LOGICAL EZERROR,EZCHEK,ACTIVE,FLGVAL,TEMP
C
      INTEGER PARAM_TYPE(MAXCPARAM),ACTION_NUMBER,SUB_MENU_CTR
      INTEGER REMNUM
      REAL    PARAM_VALUE(MAXCPARAM),OLD_VALUE(MAXCPARAM)
      CHARACTER*1  CVAL,REMARK,ACTION_REM
      CHARACTER*32 PARAM(MAXCPARAM),ACTION_COMMAND(10)
      CHARACTER*32 LAST_PACKAGE,PACKAGE,ARRAY_NAME
      CHARACTER*80 STRING
C----------------------------------------------------------------------
C
C ****  Return if command is EXIT
C
      CALL WORD(COMBINED_VIEW_COMMAND,I,J,LCOMB)
      IF ( COMBINED_VIEW_COMMAND(1:LCOMB) .EQ. 'EXIT' ) GOTO 999
C
C ****  Check if specified combined-view-command exists in the
C ****  current RCP bank
C
      IF ( .NOT. EZCHEK(COMBINED_VIEW_COMMAND(1:LCOMB)) ) THEN
        string = ' PX_COMBINE_VIEWS: Unable to find command '//
     &           COMBINED_VIEW_COMMAND(1:LCOMB)
        CALL STAMSG(STRING,.TRUE.)
        GOTO 999
      ENDIF
C
C ****  Initialize the command fifo
C
      CALL PX_ZERO_COMMAND_FIFO
C
C ****  Initialize the display action array
C
      CALL PU_INIT_DISPLAY_ACTIONS(COMBINED_VIEW_COMMAND)
C
C ****  Loop over action units of the combined command
C
      SUB_MENU_CTR = 0
      LAST_PACKAGE = CURRENT_PACKAGE
      LOLD   = 0
      PTR    = 1
      ACTIVE = .TRUE.
      DO WHILE ( ACTIVE )
        CALL PX_GET_NEXT_ACTION(COMBINED_VIEW_COMMAND(1:LCOMB),
     &                          PACKAGE,
     &                          ACTION_COMMAND,
     &                          ACTION_NUMBER,
     &                          PARAM,
     &                          PARAM_VALUE,
     &                          PARAM_TYPE,
     &                          NUMBER_PARAMS,
     &                          ACTION_REM,
     &                          PTR,IER)
C
C ****  Fill command fifo
C
        ACTIVE = IER .EQ. 0
C
C ****  Check the action's remark for its display parameter
C ****  If the remark is equal to FALSE skip this part of the
C ****  combined view.
C
        IF(( ACTION_REM .EQ. 'F' ).OR.
     &     ( ACTION_REM .EQ. 'f' ) ) THEN
          CALL PU_SET_ACTION(ACTION_COMMAND(ACTION_NUMBER),.FALSE.)
          PACKAGE = LAST_PACKAGE
          GOTO 30
        ENDIF
C
C ****  Check if package is different from last.
C ****  If so add an exit command to queue and the package name
C ****  of current view.
C
        IF ( PACKAGE .NE. LAST_PACKAGE ) THEN
C
C ****  Check if in the previous package there was a submenu(s)
C ****  If so add EXIT command(s) as necessary to get out of
C ****  submenu
C
          IF ( SUB_MENU_CTR .GT. 0 ) THEN
            DO KK = 1, SUB_MENU_CTR
              CALL PX_ADD_COMMAND('EXIT')
            ENDDO
            SUB_MENU_CTR = 0
          ENDIF
          CALL PX_ADD_COMMAND('EXIT')
          CALL PX_ADD_COMMAND(PACKAGE)
        ENDIF
C
C ****  Add the action commands to the queue
C ****  If submenu(s) keep count
C
        DO I = 1, ACTION_NUMBER
          CALL PX_ADD_COMMAND(ACTION_COMMAND(I))
          CALL SWORDS(ACTION_COMMAND(I),II,JJ,KK)
          IF ( ACTION_COMMAND(I)(JJ:JJ) .EQ. '$' )  THEN
            SUB_MENU_CTR = SUB_MENU_CTR + 1
          ELSE
C
C ****  Add the action to the display array
C
            CALL PU_SET_ACTION(ACTION_COMMAND(I),.TRUE.)
C
C ****  Add the HARDCOPY command to the queue not
C ****  a submenu and in batch mode
C
            IF( ( FLGVAL('BATCH') )  .AND.
     &        ( ( ACTION_COMMAND(I)(II:II) .NE.  '%' ) .AND.
     &          ( ACTION_COMMAND(I)(II:5)  .NE.  'DELAY' ) 
     &          .AND. (.NOT.FLGVAL('COMBINED_MODE') ) ) )THEN
              CALL PX_ADD_COMMAND('HARDCOPY')
            ENDIF
          ENDIF
   20     CONTINUE
        ENDDO
        LAST_PACKAGE = PACKAGE
   30   CONTINUE
      ENDDO
C
C ****  If the last package differs from CURRENT_PACKAGE then
C ****  force an exit from the last package, descend back to the
C ****  current package and execute the END% command.
C
      IF( CURRENT_PACKAGE .NE. LAST_PACKAGE ) THEN
C
C ****  Put as many 'EXIT's as needed according to the levels we have
C ****  found in action_commands
C
        DO I = 1, (SUB_MENU_CTR+1)
          CALL PX_ADD_COMMAND('EXIT')
        ENDDO
C
C ****  Add the current package name if different
C
        CALL PX_ADD_COMMAND(CURRENT_PACKAGE(1:LEN(CURRENT_PACKAGE)))
      ELSE
C
C ****  If there are submenus display we have to an EXIT
C ****  command so next sequence will be at the top menu
C
        DO I = 1, SUB_MENU_CTR
          CALL PX_ADD_COMMAND('EXIT')
        ENDDO
      ENDIF
      CALL PX_ADD_COMMAND('END%')
C
C ****  Activate the command fifo
C
      CALL PX_ENABLE_COMMAND_FIFO
C
  999 RETURN
      END
