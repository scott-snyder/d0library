      SUBROUTINE PU_INIT_DISPLAY_ACTIONS(ARRAY_NAME)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initializes the COMBINE_ACTIONS and DISPLAY_ACTION
C-   arrays.
C-
C-   Inputs  : ARRAY_NAME [C(*)]: Name of the combined view array
C-   Output  : None
C-
C-   ENTRY PU_SET_ACTION(ACTION_NAME,VALUE,IER)
C-   Purpose and Methods : Adds the given action name to the COMBINED_ACTION
C-   array and set its value.
C-
C-   ENTRY PU_MODIFY_DISPLAY_ACTIONS
C-   Purpose and Methods : Displays the actions in the ARRAY_NAME letting the
C-   user to modify the values.  True - displays the action, False - doesn't
C-
C-   Created  21-OCT-1992   Lupe Howell
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$OFFLINE_UTIL$GENERAL:VALUE.DEF'
      INCLUDE 'D0$PARAMS:PIXIE.DEF'
      CHARACTER*(*) ARRAY_NAME
C
      CHARACTER*80 COMBINED_ACTIONS(MAXCACT)
      CHARACTER*80 CURRENT_ARRAY_NAME,MESS
      CHARACTER*40 CDISPLAY_ACTION(MAXCACT)
      CHARACTER*1 PARAM_HELP(MAXCACT),CVAL
      LOGICAL DISPLAY_ACTION(MAXCACT),MODFLG(MAXCACT),LVAL
      INTEGER TOTAL_ACTIONS,I,J,K,L
C
      CHARACTER*(*) ACTION_NAME
      CHARACTER*(*) CURRENT_COMMAND
      INTEGER IER
      LOGICAL DISPLAY_VAL,CHANGE,TEMP,FLGVAL
      INTEGER LENGTH
C
      SAVE TOTAL_ACTIONS,COMBINED_ACTIONS,DISPLAY_ACTION
      SAVE CURRENT_ARRAY_NAME
      DATA PARAM_HELP /MAXCACT*' '/
C----------------------------------------------------------------------
C
C ****  Initialize action counter
C
      TOTAL_ACTIONS = 0
      DO I = 1, MAXCACT
        MODFLG(I) = .FALSE.
      ENDDO
      CURRENT_ARRAY_NAME = ARRAY_NAME
      GOTO 999
C#######################################################################
C-  ENTRY PU_SET_ACTION
C
      ENTRY PU_SET_ACTION(ACTION_NAME,DISPLAY_VAL)
C
      TOTAL_ACTIONS = TOTAL_ACTIONS + 1
      IF ( TOTAL_ACTIONS .LE. MAXCACT ) THEN
C
C ****  Add the action and its value to the list
C
        COMBINED_ACTIONS(TOTAL_ACTIONS) = ACTION_NAME
        DISPLAY_ACTION(TOTAL_ACTIONS) = DISPLAY_VAL
C
C ****  Set the action in the RCP bank
C ****  Check the display value and set it
C
        IF ( DISPLAY_VAL ) THEN
          CVAL = 'T'
        ELSE
          CVAL = 'F'
        ENDIF
        CALL EZ_SET_REM(CURRENT_ARRAY_NAME,'%PACKAGE',1,
     &    TOTAL_ACTIONS,CVAL,IER)
C
C ****  Error message to protect agains out of range
C
      ELSE
        CALL INTMSG
     &    (' PU_SET_ACTION - COMBINED_ACTION INDEX OUT OF RANGE !!')
      ENDIF
      GOTO 999
C#######################################################################
C-  ENTRY PU_MODIFY_DISPLAY_ACTIONS
C-
      ENTRY PU_MODIFY_DISPLAY_ACTIONS(CURRENT_COMMAND)
      CHANGE = .FALSE.
C
C ****  Check if the last command was a combined view
C ****  If it wasn't you can't remove modules
C
      IF ( INDEX(CURRENT_COMMAND,'%') .GT. 0 ) THEN
        IF ( TOTAL_ACTIONS .GT. 1 ) THEN
C
C ****  Check on the compatability in the command
C
          IF ( CURRENT_COMMAND .NE. CURRENT_ARRAY_NAME) THEN
            CALL INTMSG(' Arrays non compatible')
            GOTO 999
          ENDIF
C
C ****  Setting up the values to a string array
C
          DO I = 1, TOTAL_ACTIONS
            CALL PXGTVAL(DISPLAY_ACTION(I),VTLOG,CDISPLAY_ACTION(I))
          ENDDO
C
C ****  Display the action names with their display
C ****  value and lets use modify the values
C
          CALL MODIFY_PARAMS(TOTAL_ACTIONS,COMBINED_ACTIONS,
     &      CDISPLAY_ACTION,PARAM_HELP,MODFLG)
C
C ****  Check to see if there is any changes
C
          DO I = 1, TOTAL_ACTIONS
            IF ( MODFLG(I) ) THEN
C
C ****  IF the the value was modified change the value of in the
C ****  RCP bank using %PACKAGE
C
              CALL EZ_SET_REM(CURRENT_COMMAND,'%PACKAGE',1,I,
     &          CDISPLAY_ACTION(I),IER)
              IF ( IER .NE. 0 ) THEN
                MESS = ' Error setting the Remark'
                CALL ERRMSG('DID NOT SET_REMARK',
     &            'PU_MODIFY_DISPLAY_ACTIONS',MESS(1:25),'W')
              ENDIF
C
C ****  Modify the value in the display list
C
              CALL WORD(CDISPLAY_ACTION(I),J,K,LENGTH)
              READ(CDISPLAY_ACTION(I)(1:LENGTH),*)LVAL
              DISPLAY_ACTION(I) = LVAL
            ENDIF
C
C ****  Set the CHAGE flag
C
              IF ( .NOT. CHANGE ) CHANGE = .TRUE.
          ENDDO
C
C ****  If there was a change push the combined view command
C ****  into the command queue so the view can be redrawn
C
          CALL PU_PUSH_QUEUE(1,CURRENT_COMMAND)
        ELSE
C
C ****  Trying to remove Modules with one or less actions
C
          CALL INTMSG(' There is only one action in this view')
          CALL INTMSG(' you are not allowed to remove it')
        ENDIF
      ELSE
C
C ****  Trying to remove modules from a non combined
C ****  view
C
        CALL INTMSG
     &    (' You Can Not Remove Modules Of A Non Combined View')
      ENDIF
  999 RETURN
      END
