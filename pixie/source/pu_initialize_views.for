      SUBROUTINE PU_INITIALIZE_VIEWS (PACKAGE_NAME,COMMAND)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialize a sequence of combined view
C-   commands or window variables for a single view.
C-
C-   Inputs  : PACKAGE_NAME     [C*]    Name of package containing
C-                                      combined-view-command
C-             COMMAND          [C*]    Combined view command
C-   Outputs : None
C-   Controls: None
C-
C-   Entry points:
C-
C-      CALL PU_MODIFY_VIEW(PACKAGE_NAME,COMMAND,IER)
C-      CALL PU_RESET_VIEWS
C-
C-   Created  25-SEP-1990   Harrison B. Prosper
C-   Updated  27-SEP-1990   Harrison B. Prosper
C-   Updated  11-OCT-1990   Lupe Howell  PU_MODIFY_VIEW and PU_RESET_VIEWS
C-                          was debugged
C-   Updated  16-OCT-1990   Lupe Howell and Harrison B. Prosper
C-   Updated  14-JAN-1991   Harrison B. Prosper
C-      Use PXSCREEN and PXPARMS
C-   Updated  28-JAN-1991   Lupe Howell  PU_MODIFY_VIEW Window_Saved check
C-      making sure that the window parameters are saved.
C-   Updated   6-MAY-1991   Harrison B. Prosper
C-      Re-introduce command mis-match check;
C-      Add PACKAGE_NAME to PU_MODIFY_VIEW
C-   Updated  15-MAY-1991   Harrison B. Prosper
C-      Change arguments in PU_GOTO_SCREEN
C-   Updated  17-SEP-1991   Lupe Howell Ignore initialization of 
C-      PU_SAVE_SCREEN_BEGIN when INITIALIZED is True.  
C-   Updated  24-JAN-1992   Lupe Howell  Updated for SGI 
C-   Updated  17-JUL-1992   Lupe Howell  Allow modifycation of character var
C-   Updated  22-OCT-1992   Lupe Howell  Updating call to PX_GET_NEXT_ACTION 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) PACKAGE_NAME
      CHARACTER*(*) COMMAND
      INTEGER IER
C----------------------------------------------------------------------
      INCLUDE 'D0$OFFLINE_UTIL$GENERAL:VALUE.DEF'
      INCLUDE 'D0$INC:PXCOMK.INC'
      INCLUDE 'D0$PARAMS:PIXIE.DEF'
C----------------------------------------------------------------------
      INTEGER NUMBER_PARAMS,PTR,I,J,K,L,ERROR
      INTEGER LCOMB,LCOM,LPACK,ITYPE,LC,LP
C
      INTEGER IVALUE
      REAL    RVALUE
      LOGICAL LVALUE
      EQUIVALENCE(IVALUE,RVALUE,LVALUE)
C
      LOGICAL EZERROR,EZCHEK,PU_SET_RCP_BANK
      LOGICAL COMBINED_MODE
      LOGICAL FLGVAL,INITIALIZED,PARAM_FLAG
      LOGICAL MATCHED,GET_NEXT_ACTION
C
      INTEGER PARAM_TYPE(MAXCPAR)
      REAL    PARAM_VALUE(MAXCPAR)
      CHARACTER*32 PARAM(MAXCPAR),ACTION(MAXCACT)
C
      INTEGER TOTAL_BLOCKS,III,BLOCK,SCREEN_IDX 
      INTEGER VALTYPE,NUMBER_ACTIONS

      CHARACTER*32 RCPBANK,PACKAGE_USE,ARRAY_NAME
      CHARACTER*32 OLD_COMMAND, OLD_COMBINED_VIEW,OLD_RCPBANK
      CHARACTER*32 OLD_PACKAGE_NAME
      CHARACTER*32 BANK_NAME
      CHARACTER*40 REM
      CHARACTER*80 MESS
C
      INTEGER LKEY,IDX
C----------------------------------------------------------------------
      SAVE OLD_RCPBANK,OLD_COMMAND, OLD_COMBINED_VIEW,PTR,
     &     LPACK,LCOMB,OLD_PACKAGE_NAME
C----------------------------------------------------------------------
C
C ****  Ignore Initialization for PU_SAVE_SCREEN if INITIALIZED has not 
C ****  been reset and it still on.
C
      IF ( INITIALIZED ) GOTO 888
      CALL PU_SAVE_SCREEN_BEGIN
  888 CONTINUE
C
C ****  Set flag to indicate that initialization done.
C
      INITIALIZED = .TRUE.
C
      PTR   = 1                        ! Go to start of array
C
C ****  Note name of original package
C
      CALL WORD(PACKAGE_NAME,I,J,LPACK)
      OLD_RCPBANK       = 'PX_'//PACKAGE_NAME(1:LPACK)//'_RCP'
      OLD_PACKAGE_NAME  = PACKAGE_NAME
      OLD_COMBINED_VIEW = OLD_COMMAND
      OLD_COMMAND       = COMMAND
      CALL WORD(OLD_COMMAND,I,J,LCOMB)
      RETURN
C
      ENTRY PU_MODIFY_VIEW(PACKAGE_NAME,COMMAND,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Search the RCP bank associated with the
C-   current package for the specified combined view command and
C-   modify the parameters in PXSCREEN and PXPARAMS corresponding
C-   to the given single view command. Use entry point
C-
C-      PU_RESET_VIEWS
C-
C-   to reset the parameters to their original values. However if
C-   we are in single view mode we should save only the window
C-   parameters (no parameters are modified in this case).
C-
C-   Purpose and Methods :
C-
C-   Inputs  : PACKAGE_NAME     [C*]    Name of package
C-             COMMAND          [C*]    Name of single view command
C-   Outputs : IER              [I]     Error code
C-   Controls:
C-
C-   Created  25-SEP-1990   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      CALL EZTELL(BANK_NAME,L)          ! Get current RCP-bank
      COMBINED_MODE = FLGVAL('COMBINED_MODE')
C
C ****  Set to the original RCP bank
C
      CALL EZPICK(OLD_RCPBANK)
      IF ( EZERROR(ERROR) ) THEN
        MESS = ' PU_MODIFY_VIEW: Unable to EZPICK '//OLD_RCPBANK
        CALL INTMSG(MESS) 
        GOTO 999
      ENDIF
C
C ****  Clear the screen number store. Note for single view mode
C ****  the combined view command will in fact be the current
C ****  command
C
      CALL EZTELL(BANK_NAME,L)
C
C ****  Check if specified combined-view-command exists in the
C ****  current RCP bank
C
      IF ( COMBINED_MODE ) THEN
        IF ( .NOT. EZCHEK(OLD_COMMAND(1:LCOMB)) ) THEN
          MESS = ' PU_MODIFY_VIEW: Unable to find command '//
     &           OLD_COMMAND(1:LCOMB)
          CALL INTMSG(MESS)
          GOTO 900
        ENDIF
C
C ****  Get next action command; check if action is the
C ****  same as COMMAND. It should be!!
C
        CALL WORD(PACKAGE_NAME(1:LEN(PACKAGE_NAME)),I,J,LP)
        CALL WORD(COMMAND(1:LEN(COMMAND)),I,J,LC)
C
        MATCHED = .FALSE.
        GET_NEXT_ACTION = .TRUE.
        DO WHILE ( GET_NEXT_ACTION )
C
          CALL PX_GET_NEXT_ACTION(OLD_COMMAND(1:LCOMB),
     &                            PACKAGE_USE,
     &                            ACTION,
     &                            NUMBER_ACTIONS,
     &                            PARAM,
     &                            PARAM_VALUE,
     &                            PARAM_TYPE,
     &                            NUMBER_PARAMS,
     &                            REM,
     &                            PTR,IER)
C
C ****  Check package and action command
C
          GET_NEXT_ACTION = IER .EQ. 0
          IF ( PACKAGE_NAME(1:LP) .EQ. PACKAGE_USE(1:LP) ) THEN
            IF ( COMMAND(1:LC) .EQ. ACTION(NUMBER_ACTIONS)(1:LC) ) THEN
              MATCHED = .TRUE.
              GET_NEXT_ACTION = .FALSE.
            ENDIF
          ENDIF
        ENDDO
C
C ****  Must find package/action !
C
        IF ( .NOT. MATCHED ) THEN
          MESS = ' Could NOT find /'//PACKAGE_NAME(1:LP)//'/'//
     &           COMMAND(1:LC)//'/ in '//OLD_COMMAND(1:LCOMB)
          CALL ERRMSG('NO_ACTION','PU_MODIFY_VIEW',MESS,'W')
          GOTO 900
        ENDIF
C
      ELSE
C
C ****  Initialize variables for single view mode
C
        PACKAGE_USE    = OLD_PACKAGE_NAME
        NUMBER_ACTIONS  = 1
        ACTION(NUMBER_ACTIONS) = COMMAND
      ENDIF
C
C ****  Modify parameters pertaining to ACTION
C ****  Pick the correct RCP bank (the one containing
C ****  the current action command. Build names of Screen
C ****  and Parameter arrays for current package
C
      IF ( NUMBER_ACTIONS .GT. 0 ) THEN
C
        IF ( .NOT. PU_SET_RCP_BANK(PACKAGE_USE) ) THEN
          MESS = 
     &      ' PU_MODIFY_VIEW: Unable to pick bank for '//PACKAGE_USE
          CALL INTMSG(MESS)
          GOTO 900
        ELSE
C
C ****  We now have both the package and the action; so find
C ****  screen index IDX and save the viewing
C
          CALL PU_GET_SCREEN_INDEX(ACTION(NUMBER_ACTIONS),IDX,IER)
C
C ****  Save current viewing parameters
C
          CALL PU_SAVE_SCREEN(PACKAGE_USE,IDX)
          SCREEN_IDX = IDX
C
C ****  Execute this code only for combined commands
C
          IF ( COMBINED_MODE ) THEN
            DO I = 1, NUMBER_PARAMS
C 
              IF ( PARAM(I)(1:1) .EQ. '%' ) THEN
                IF( PARAM(I)(1:8) .EQ. SCREEN_KEY ) THEN
                  ARRAY_NAME = 'PXSCREEN'
                  PARAM_FLAG = .FALSE.
                  IDX = SCREEN_IDX
                ELSEIF( PARAM(I)(1:8) .EQ. PARAM_KEY) THEN
                  ARRAY_NAME = 'PXPARAMS'
                  PARAM_FLAG = .TRUE.
                  IDX = 1
                ELSE
                  MESS = ' PU_RESET_VIEWS: Bad % command in ARRAY '//
     &                   RCPBANK   
                  CALL INTMSG(MESS)
                ENDIF
              ELSE
C
C ****  Check for key flag PARAM_FLAG to save the parameters
C
                IF ( PARAM_FLAG ) THEN
                  CALL PU_SAVE_PARAM(PACKAGE_USE,PARAM(I))
                ENDIF
C
C ****  Use equivalences to set correct type
C
                RVALUE = PARAM_VALUE(I)
                IF     ( PARAM_TYPE(I) .EQ. VTINT ) THEN
                  CALL EZ_SET_ELEMENT(ARRAY_NAME,PARAM(I),IDX,1,
     &               IVALUE,IER)
                ELSEIF ( PARAM_TYPE(I) .EQ. VTREAL ) THEN
                  CALL EZ_SET_ELEMENT(ARRAY_NAME,PARAM(I),IDX,1,
     &               RVALUE,IER)
                ELSEIF ( PARAM_TYPE(I) .EQ. VTLOG ) THEN
                  CALL EZ_SET_ELEMENT(ARRAY_NAME,PARAM(I),IDX,1,
     &               LVALUE,IER)
                ELSEIF ( PARAM_TYPE(I) .GE. VTCHAR ) THEN
                  CALL EZ_SET_ELEMENT(ARRAY_NAME,PARAM(I),IDX,1,
     &              IVALUE,IER)
                ENDIF
                IF ( IER .NE. 0 ) THEN
                  MESS = ' PU_MODIFY_VIEW: Unable to set '//PARAM(I)
                  CALL INTMSG(MESS)
                ENDIF
              ENDIF
            ENDDO
          ENDIF
C
          CALL EZTELL(BANK_NAME,L)
          CALL EZRSET                   ! Reset RCP bank
          CALL EZTELL(BANK_NAME,L)
        ENDIF
      ENDIF
C
C ****  First Reset RCP bank before exit
C
      GOTO 900
C
      ENTRY PU_RESET_VIEWS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Reset modified view parameters to their
C-   original values.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  25-SEP-1990   Lupe Howell, Harrison B. Prosper
C-
C----------------------------------------------------------------------
C
C ****  Ignore reset if PU_INITIALIZED has not been called.
C
      IF ( INITIALIZED ) THEN
        INITIALIZED = .FALSE.
      ELSE
        GOTO 999                        ! Not initialized
      ENDIF
C
      COMBINED_MODE = FLGVAL('COMBINED_VIEW')
C
C ****  Turn off zooming and Rotating
C
      CALL FLGSET('ZOOMING',.FALSE.)
      CALL FLGSET('ROTATING',.FALSE.)
C
C ****  Pick RCP bank containing combined-view-commands
C
      CALL EZPICK(OLD_RCPBANK)
      IF ( EZERROR(ERROR) ) THEN
        MESS = ' PU_RESET_VIEWS: Unable to EZPICK '//OLD_RCPBANK
        CALL INTMSG(MESS)
        GOTO 999
      ENDIF
C
C ****  Check if specified combined-view-command exists in the
C ****  current RCP bank
C
      IF ( COMBINED_MODE ) THEN
        IF ( .NOT. EZCHEK(OLD_COMMAND(1:LCOMB)) ) THEN
          MESS = ' PU_RESET_VIEWS: Unable to find command '//
     &           OLD_COMMAND(1:LCOMB)
          CALL INTMSG(MESS)
          GOTO 900
        ENDIF
      ENDIF
C
C ****  Loop over blocks to be restored
C
      CALL PU_GET_SAVED_BLOCK_COUNT(TOTAL_BLOCKS)
      DO III = 1, TOTAL_BLOCKS
        BLOCK = III
        CALL PU_GET_SAVED_PACKAGE(BLOCK,PACKAGE_USE)
C
C ****  Reset parameters pertaining to current block 
C ****  Pick the correct RCP bank (the one containing
C ****  the current action command. 
C
        IF ( .NOT. PU_SET_RCP_BANK(PACKAGE_USE) ) THEN
          MESS = ' PU_RESET_VIEWS: Unable to pick bank for '//
     &           PACKAGE_USE
          CALL INTMSG(MESS)
          GOTO 900
        ENDIF
C
C ****  Restore current screen and parameters
C
        CALL PU_RESTORE_SCREEN(BLOCK)
        CALL PU_RESTORE_PARAM(BLOCK)
C
        CALL PU_RESET_RCP_BANK                   ! Reset RCP bank
      ENDDO
C
C ****  Reset RCP bank
C
  900 CONTINUE
      CALL EZTELL(BANK_NAME,L)
      CALL PU_RESET_RCP_BANK
      CALL EZTELL(BANK_NAME,L)
  999 RETURN
      END
