      SUBROUTINE PXMOD_COMBINED_VIEWS(LAST_COMMAND,MODIFY_VIEW,ICOM,
     &  COMBINE_ARRAY,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Display the names of combined-view arrays.
C-   If the MODIFY_VIEW flag is on the display of all the combined views
C-   will be skipped using LAST_COMMAND as the combined display to modify.
C-   Once a combined view is chosen either by the user or by LAST_COMMAND
C-   a list of the available packages that belong to that view will be
C-   displayed to choose from.
C-   This routine returns the menu index corresponding to the chosen
C-   parameter to modify (SCREEN or PARAMETERS)
C-
C-   Inputs  : LAST_COMMAND[C(*)]: Last valid command given by the user if any
C-             MODIFY_VIEW    [L]: Flag for skipping the display of all the
C-                                combined views available.
C-
C-   Outputs : ICOM          [I]: Number of command requested
C-                                1-PARAMETERS, 2-SCREEN
C-             COMBINE_ARRAY[C*]: Name of the combined array chosen if any
C-             IER        [I   ]: Error flag
C-
C-   Controls: None
C-
C-   Entries:
C-      ENTRY PXMOD_COMBINED_ARRAY(ARRAYNAME,PARNAM,PARCVAL,PARTY,IER)
C-        Modifies a given parameter in the combined array if it is found.
C-
C-      ENTRY PXGET_COMBINED_PARAM(COMBARR,PARNAMES,PARVAL,TOTPAR,IER)
C-      Given a list of parameters and their values the routine checks if
C-      the combined view selected has any of these parameters and if it
C-      does replace its value with the value in the combined view.
C-
C-      ENTRY PXGET_COMBINED_ACTION(TOTACT,ACTIONS)
C-      Returns action(s) and total number of actions that belong to the
C-      requested combined view package.
C-
C-      ENTRY PXCOMB_VIEW(COMB,COMBARR)
C-      Returns a logical flag COMB .TRUE. a combined view was selected
C-      for modification COMB .FALSE. a combined view was NOT selected for
C-      modification
C-
C-      ENTRY PXCOMB_RESET_MOD sets the COMBINED variable to false to indicate
C-      the modifications to a combined view are over.
C-
C-   Created  13-MAR-1991   LUPE HOWELL
C-   Updated  20-MAR-1991   Harrison B. Prosper
C-      Bug fix
C-   Updated  29-MAR-1991   Lupe Howell  Bug fix
C-   Updated   8-OCT-1991   Lupe Howell Entries PXMOD_COMBINED_ARRAY,
C-      PXGET_COMBINED_PARAM,PXGET_COMBINED_ACTION,PXCOMB_VIEW
C-   Updated  10-JAN-1992   Lupe Howell  Modify call to PX_DISPLAY_ITEMS
C-   Updated  27-JAN-1992   Lupe Howell  Update for SGI
C-   Updated  19-MAR-1992   Lupe Howell  ENTRY PXCOMB_RESET_MOD
C-   Updated  31-MAR-1992   Lupe Howell  Tidy up
C-   Updated  22-OCT-1992   Lupe Howell  Updated PX_GET_NEXT_ACTION call
C-   Updated   2-NOV-1992   Lupe Howell  Add DISPLAY_ITEMS
C-   Updated   8-JUL-1993   Lupe Howell  Display the SYSTEM package when modify
C-          a combined view 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) LAST_COMMAND
      CHARACTER*(*) COMBINE_ARRAY
      INTEGER ICOM
      LOGICAL MODIFY_VIEW
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:PIXIE.DEF'
      INCLUDE 'D0$INC:PXCOMK.INC'
C----------------------------------------------------------------------
      CHARACTER*40 PARAM(MAXCPAR),ACTION_COMMAND(MAXCPAR),PACKAGE_USE
      CHARACTER*40 PACKAGES_LIST(MAXCPAR),ALL_PACKAGES(MAXCPAR)
      CHARACTER*40 PARA_CVAL(MAXCPAR),ALL_ACTIONS(MAXCPAR),REM
      CHARACTER*80 MESS
C
      INTEGER PARA_VALUE(MAXCPAR),PARA_TYPE(MAXCPAR)
      INTEGER PARAM_TYPE(MAXCPAR),NUMBER_PARAMS,PTR
      INTEGER PARAM_VALUE(MAXCPAR),NEXT,ALEN,LPACK,FLEN,PARCOM
C
      INTEGER INDX(MAXCVIEW),COUNT,IER,I,J,K,OUTNUM
      INTEGER ACTION_NUMBER,TOTAL_VIEWS,VAL_COUNT
C
      LOGICAL OK,ACTIVE,COMBINED,FLAG,MODIFICATION
C
      INTEGER PARNUM
      PARAMETER( PARNUM = 2 )
      CHARACTER*40 PAROPTION(PARNUM),REM_OPTION(PARNUM)
      DATA PAROPTION/'PARAMETERS','SCREENS'/
      DATA REM_OPTION/'Modify General Parameters',
     &                'Modify Screen Parameters'/
C
      SAVE OUTNUM,ALL_PACKAGES,ALL_ACTIONS,PACKAGES_LIST,INDX,COMBINED
      SAVE PARCOM
C----------------------------------------------------------------------
      CHARACTER*(*) ARRAYNAME
      CHARACTER*(*) PARNAM
      CHARACTER*(*) PARCVAL(*)
      INTEGER PARTY
C
      CHARACTER*40 PACKAGE_CHOSEN,COMBINE_RCP
      LOGICAL ERRFLG
      INTEGER SCRENUM,N
C----------------------------------------------------------------------
      CHARACTER*(*) COMBARR
      CHARACTER*(*) PARNAMES(*)
      INTEGER PARVAL(*)
C
      CHARACTER*40 CVAL
      INTEGER TOTPAR
C----------------------------------------------------------------------
      INTEGER TOTACT
      CHARACTER*(*) ACTIONS(*)
C----------------------------------------------------------------------
      LOGICAL COMB
C----------------------------------------------------------------------
      CALL EZTELL(COMBINE_RCP,N)
      COMBINED = .FALSE.
C
C ****  Get the all the combine view arrays' names and
C ****  Get option from user
C
      IF ( MODIFY_VIEW ) THEN
        COMBINE_ARRAY = LAST_COMMAND
        IER = 0
      ELSE
        CALL PXGET_COMBINED_VIEWS(COMBINE_ARRAY,IER)
      ENDIF
      IF ( IER .EQ. 0 ) THEN
        PTR = 1
        IER = 0
        COUNT = 0
        TOTAL_VIEWS = 0
        VAL_COUNT = 0
C
C ****  Getting all the available packages in the chosen combined array
C
        DO WHILE ( IER .EQ. 0 )
          TOTAL_VIEWS = TOTAL_VIEWS + 1
          COUNT = COUNT + 1
          INDX(TOTAL_VIEWS) = PTR
          CALL PX_GET_NEXT_ACTION(COMBINE_ARRAY,
     &                            PACKAGE_USE,
     &                            ACTION_COMMAND,
     &                            ACTION_NUMBER,
     &                            PARAM,
     &                            PARAM_VALUE,
     &                            PARAM_TYPE,
     &                            NUMBER_PARAMS,
     &                            REM,
     &                            PTR,IER)

          ALL_PACKAGES(TOTAL_VIEWS) = PACKAGE_USE
          ALL_ACTIONS(TOTAL_VIEWS)  = ACTION_COMMAND(ACTION_NUMBER)
C
C ****  Check for repeated package name. Do not put it list
C ****  if repearted
C
          I = 1
          OK = .TRUE.
          ACTIVE = .TRUE.
          IF( COUNT .GT. 1 ) THEN
            DO WHILE ( ACTIVE )
              IF ( PACKAGES_LIST(I) .EQ. PACKAGE_USE ) THEN
                OK = .FALSE.
              ELSE
                I = I + 1
                ACTIVE = (I .LT. COUNT)
              ENDIF
              ACTIVE = OK .AND. ACTIVE
            ENDDO
          ENDIF
          IF( OK ) THEN
            PACKAGES_LIST(COUNT) = PACKAGE_USE ! add the package name no repeat
          ELSE
            COUNT = COUNT - 1 !Decrease the count
          ENDIF
        ENDDO
        COUNT = COUNT + 1
        PACKAGES_LIST(COUNT) = 'SYSTEM'
C
C ****  Checking for errors during reading the combined array
C
        IF ( IER .EQ. -1 ) THEN ! Error
          GOTO 999
        ELSE
          IER = 0
        ENDIF
C
C ****  List of packages in chosen combined display and getting the
C ****  user's choice
C
        MESS = ' PACKAGES IN '//COMBINE_ARRAY
        CALL STAMSG(MESS,.TRUE.)
C
C ****  Selecting the chosen package
C
        CALL PX_SELECT_PACKAGE(PACKAGES_LIST,COUNT,OUTNUM)

        IF ( OUTNUM .NE.0 ) THEN
C
C ****  Selecting what kind of parameters to display(PARAMETERS/SCREEN)
C ****  If a selection was made return the selection
C ****  If no selection was made check the list of packages
C ****  if the number of packages is 1 return 0 to exit modify
C
          CALL WORD(PACKAGES_LIST(OUTNUM),I,J,K)
          MESS = ' PACKAGE '//PACKAGES_LIST(OUTNUM)(1:K)
          CALL STAMSG(MESS,.TRUE.)
          MESS = 'PARAMETERS FOR  '//COMBINE_ARRAY
          CALL DISPLAY_ITEMS(
     &      PARNUM,PAROPTION,REM_OPTION,MESS,PARCOM)
          IF ( PARCOM .NE. 0 ) THEN
            ICOM = PARCOM
            COMBINED = .TRUE.
C
C ****  If no Option made (PARAMETERS/SCREEN) reset the package if
C ****  there was a package picked.
          ELSE
            CALL PX_CHECK_PICK(FLAG)  ! Checking if package set
            IF ( FLAG ) THEN
              CALL PX_RESET_SELECT_PACKAGE
              CALL PX_SET_PACKAGE(COMBINE_RCP) ! Set combine RCP if was reset
            ENDIF
            IF( (COUNT .EQ. 1) .AND. (MODIFY_VIEW) )
     &        ICOM = 0  ! Exit Modify menu
          ENDIF
        ELSE
C ****  If no package was selected and we are in MODIFY_VIEW mode
C ****  we should exit the modify menu by setting ICOM  to 0
C
          IF ( MODIFY_VIEW )
     &      ICOM = 0                 ! Exit Modify menu
        ENDIF
      ENDIF
  999 RETURN
C#######################################################################
      ENTRY PXMOD_COMBINED_ARRAY(ARRAYNAME,PARNAM,PARCVAL,
     &  PARTY,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Modifies a given parameter in the combined
C-   array if it is found.
C-
C-   Inputs  : ARRAYNAME  [C*]: Name of the combined array
C-             PARNAM     [C*]: Name of the parameter to modify
C-             PARCVAL    [C*]: New value of the parameter
C-             PARTY      [I ]: Type of the parameter
C-
C-   Outputs : IER        [I ]: Error flag 0 Ok
C-                              -1 If the parameter is not found in the
C-                                 combined view
C-
C-   Created  10-OCT-1991   Lupe Howell
C-
C----------------------------------------------------------------------
      IER = 0
      PACKAGE_CHOSEN = PACKAGES_LIST(OUTNUM)
      MODIFICATION = .FALSE.
      ERRFLG = .FALSE.
C
C ****  Checking if modifying a screen.  If a screen is modify
C ****  get the index of the screen and modify the parameter only on
C ****  that view
C
      IF ( PARCOM .EQ. 2 ) THEN
        CALL PXGET_IDX_SCREEN(SCRENUM)
        IF ( PACKAGE_CHOSEN .EQ. ALL_PACKAGES(SCRENUM) ) THEN
          CALL PXRDVAL
     &      (PARNAM,PARCVAL,PARTY,ARRAYNAME,INDX(SCRENUM),IER)
          IF( IER .NE. 0 ) THEN
            ERRFLG = .TRUE.
          ELSE
            MODIFICATION = .TRUE.
          ENDIF
        ENDIF
      ELSE
C
C ****  If parameters are being modify Search for the parameter
C ****  in all the views that matched with the chosen package.
C
        DO I = 1, TOTAL_VIEWS
          IF( PACKAGE_CHOSEN .EQ. ALL_PACKAGES(I) ) THEN
            CALL PXRDVAL
     &        (PARNAM,PARCVAL,PARTY,ARRAYNAME,INDX(I),IER)
            IF( IER .NE. 0 ) THEN
              ERRFLG = .TRUE.
            ELSE
              MODIFICATION = .TRUE.
            ENDIF
          ENDIF
        ENDDO
      ENDIF
      IF ( .NOT. MODIFICATION ) THEN
        IF ( PACKAGE_CHOSEN .EQ. 'SYSTEM' ) THEN
          CALL EZPICK('PX_SYSTEM_RCP')
          CALL PXRDVAL(PARAM,PARCVAL,PARTY,'PXPARAMS',1,IER)
          CALL EZRSET
        ENDIF
      ENDIF
      IF ( ERRFLG ) THEN
        IER = -1
      ENDIF
      RETURN
C#######################################################################
      ENTRY PXGET_COMBINED_PARAM(COMBARR,PARNAMES,PARVAL,TOTPAR,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Given a list of parameters and their values the
C-   routine checks if the combined view selected has any of these
C-   parameters and if it does replace its value with the value in the
C-   combined view.
C-
C-   Inputs  : COMBARR[C(*)]: Name of the combined array
C-             PARNAMES[C(*)]: Names of the parameters to compare
C-             PARVAL  [I(*)]: Values of the parameters
C-             TOTPAR     [I]: Total number of parameters
C-
C-   Outputs : PARVAL  [I(*)]: Values of the parameters found in the
C-                             combined view
C-             IER        [I]: Error flag 0 OK otherwise, problems reading
C-                             combined array
C-
C-   Created  10-OCT-1991   Lupe Howell
C-
C----------------------------------------------------------------------
      IER = 0
      J = 0
      PACKAGE_CHOSEN = PACKAGES_LIST(OUTNUM)
      PTR = INDX(OUTNUM)
      IF ( PTR .GT. 0 ) THEN
C
C ****  Getting the parameters of the chosen combined
C ****  view
C
        CALL PX_GET_NEXT_ACTION(COMBARR,
     &                        PACKAGE_USE,
     &                        ACTION_COMMAND,
     &                        ACTION_NUMBER,
     &                        PARAM,
     &                        PARAM_VALUE,
     &                        PARAM_TYPE,
     &                        NUMBER_PARAMS,
     &                        REM,
     &                        PTR,IER)
C
C ****  Check error code tosee if it was the last element
C ****  in the array.  If the last element set error code to 0
C
        IF ( IER .EQ. 1 ) THEN
          IER = 0
        ENDIF
C
C ****  Comparing the parameters in the combined view with
C ****  the input parameters
C
        DO WHILE ( J .LE. NUMBER_PARAMS )
          J = J + 1
          I = 0
          ACTIVE = .TRUE.
          IF( PARAM(J)(1:1) .EQ. '%' )THEN  ! Skipp the search if %* command
            ACTIVE = .FALSE.
          ENDIF
          DO WHILE ( ACTIVE )
            I = I + 1
C
C ****  If a match found replace give the value of the
C ****  combined view parameter
C
            IF( PARAM(J) .EQ. PARNAMES(I) ) THEN
              PARVAL(I) = PARAM_VALUE(J)
              ACTIVE = .FALSE.
            ENDIF
            ACTIVE = ACTIVE .AND. ( I .LE. TOTPAR )
          ENDDO
        ENDDO
      ELSE
        IER = -1
      ENDIF
      RETURN
C#######################################################################
      ENTRY PXGET_COMBINED_ACTION(TOTACT,ACTIONS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns action(s) and total number of actions
C-   that belong to the requested combined view package.
C-
C-   Inputs  : None
C-
C-   Outputs : TOTACT     [I]: Total number of actions
C-             ACTIONS[C*(*)]: Names of the actions
C-
C-   Created  11-OCT-1991   Lupe Howell
C-
C----------------------------------------------------------------------
      PACKAGE_CHOSEN = PACKAGES_LIST(OUTNUM)
      J = 0
      DO I = 1, TOTAL_VIEWS
        IF( ALL_PACKAGES(I) . EQ. PACKAGE_CHOSEN ) THEN
          J = J + 1
          ACTIONS(J) = ALL_ACTIONS(I)
        ENDIF
      ENDDO
      TOTACT = J
      RETURN
C#######################################################################
      ENTRY PXCOMB_VIEW(COMB,COMBARR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns a logical flag
C-         COMB .TRUE. a combined view was selected for modification
C-         COMB .FALSE. a combined view was NOT selected for modification
C-
C-   Inputs  : None
C-   Outputs : COMB    [L]:
C-             COMBARR[C*]:
C-
C-   Created  11-OCT-1991   Lupe Howell
C-
C----------------------------------------------------------------------
      COMB = COMBINED
      COMBARR = COMBINE_ARRAY
      RETURN
C#######################################################################
      ENTRY PXCOMB_RESET_MOD
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Sets the COMBINED variable to false
C-
C-   Inputs  :
C-   Outputs :
C-
C-   Created  19-MAR-1992   Lupe Howell
C-
C----------------------------------------------------------------------
      IF ( COMBINED )
     &  COMBINED = .FALSE.
      RETURN
      END
