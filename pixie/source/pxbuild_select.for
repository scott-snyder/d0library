      SUBROUTINE PXBUILD_SELECT(COMMAND)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Show MENU of available action routines.
C-
C-   Inputs  : COMMAND     [C*]    Name of the RCP file selecting
C-   Outputs : None
C-   Controls:
C-
C-   Created  12-SEP-1990   Harrison B. Prosper
C-   Updated   1-MAY-1991   Lupe Howell  Maneging submenus
C-   Updated  19-JUN-1991   Lupe Howell  Add a NEW screen option added
C-   Updated  31-JUL-1991   Lupe Howell  A new parameter was added to the call
C-                                 to PXBUILD_ADD_NEW_ITEM
C-   Updated   2-DEC-1991   Lupe Howell  Getting information from PX_*_RCP
C-   Updated  27-JAN-1992   Lupe Howell  Update for SGI
C-   Updated  16-MAR-1992   Lupe Howell  Tidy up
C-   Updated   2-NOV-1992   Lupe Howell  Add DISPLAY_ITEMS
C-   Updated  29-JAN-1993   Lupe Howell  Fix GOTO statement outside block
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) COMMAND
C
      INTEGER MAXROUTINES
      PARAMETER( MAXROUTINES = 200 )
C
      CHARACTER*(*) ORIGINAL_RCP
      PARAMETER( ORIGINAL_RCP = 'ORIGINAL_RCP' )
C
      CHARACTER*(*) TEMP_RCP
      PARAMETER( TEMP_RCP = 'TEMPORARY_NAME' )
C
      CHARACTER*32 ROUTINE(MAXROUTINES)
      CHARACTER*40 COMMAND1,COMMAND2,SUBMENU_NAME,SUBMENU_LIST(10)
      CHARACTER*40 SUBMENU_REM(10),REMOVE_REM(MAXROUTINES)
      CHARACTER*80 NAME1(MAXROUTINES),NAME2(MAXROUTINES),MENU,STRING
      CHARACTER*5  NUMSTR
C
      INTEGER I,J,K,N,LMENU,LCOM,LSMENU,IER,IDX,NROUTINES,MENU_INDX
      INTEGER II,JJ,KK,CL,RL,REMREC(MAXROUTINES),ICOM,CLEN
C
      LOGICAL COMPRESS,NEW,FOUND,MATCH
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:PXBUILDCOM.INC' 
C----------------------------------------------------------------------
      DATA REMOVE_REM /MAXROUTINES*' Remove Element '/
C----------------------------------------------------------------------
C
      I = INDEX(COMMAND,'PX_')
      J = INDEX(COMMAND,'_RCP') - 1
      IF ( I .EQ. 0 )
     &  I = 1
      MENU = COMMAND(I:J)
      CALL WORD(MENU,I,J,LMENU)
      STRING = ' EDITING MENU '//COMMAND
      CALL STAMSG(STRING,.TRUE.)
      CALL WORD(COMMAND,I,J,CLEN)
C
C ****  Save the original RCP file if CLEAR is chosen
C
      I = 0
      J = 0
      CALL EZCOPY(COMMAND,ORIGINAL_RCP,I,J,IER)
      COMMAND1 = ' '
C
C ****  Determine the index corresponding to the menu selected
C
      MENU_INDX = 0
      FOUND = .FALSE.
      DO WHILE ( ( MENU_INDX .LT. MENU_COUNT ) .AND. ( .NOT. FOUND ) )
        MENU_INDX = MENU_INDX + 1
        IF ( MENU(1:LMENU) .EQ. MENU_NAME(MENU_INDX)(1:LMENU) ) THEN
          FOUND = .TRUE.
        ENDIF
      ENDDO
      IF ( .NOT. FOUND ) THEN
        STRING = ' Could NOT MENU Name '//MENU
        CALL INTMSG( STRING )
        GOTO 999
      ENDIF
C
C ****  If more than one submenu pick the one to edit
C
      DO I = 1, SUBMENU_COUNT(MENU_INDX)
        SUBMENU_LIST(I) = ACTION_SUBMENU(MENU_INDX,I)
        CALL WORD(SUBMENU_LIST(I),J,K,II)
        SUBMENU_REM(I)  = ' Select package '//SUBMENU_LIST(I)(J:K)
      ENDDO
      ICOM = -1
      DO WHILE ( ICOM .NE. 0 )
        IF ( SUBMENU_COUNT(MENU_INDX) .GT. 1 ) THEN
          CALL DISPLAY_ITEMS
     &      (SUBMENU_COUNT(MENU_INDX),SUBMENU_LIST,
     &      SUBMENU_REM,'SELECT SUBMENU',ICOM)
C
C ****  If only one submenu skip display of submenus for selection
C
        ELSE
          ICOM = 1
        ENDIF
        IF ( ICOM .GT. 0 ) THEN
          SUBMENU_NAME = SUBMENU_LIST(ICOM)
          IDX = ICOM
        ELSE
          GOTO 800
        ENDIF
   10   CONTINUE
C
        CALL WORD(SUBMENU_NAME,I,J,LSMENU)
C
C ****  Display the EDIT MENUS options
C
        CALL WORD(MENU,I,J,LSMENU)
        COMMAND2 = ' '
        DO WHILE ( COMMAND2 .NE. 'EXIT' )
          STRING = 'Editing Menus for '//COMMAND
          CALL SWORDS(STRING,I,J,K)
          CALL MENUDO(STRING(I:J),
     &          'MENU_EDITING',COMMAND2)
C
C ****  LIST
C
          IF     ( COMMAND2 .EQ. 'LIST' ) THEN
            IF ( ACTION_COUNT(MENU_INDX,IDX) .GT. 0 ) THEN
              CALL INTMSG(' ')
              DO I =  1, ACTION_COUNT(MENU_INDX,IDX)
                NUMSTR = ' '
                CALL INTSTR(I,'. ',NUMSTR,N)
                STRING =
     &                 ' '//NUMSTR//'<'//ACTION_ITEM(MENU_INDX,IDX,I)//
     &                 '>'//ACTION_NAME(MENU_INDX,IDX,I)
                CALL INTMSG(STRING)
              ENDDO
            ELSE
              CALL INTMSG (' No action routines have been selected')
            ENDIF
C
C ****  REMOVE
C
          ELSEIF ( COMMAND2 .EQ. 'REMOVE' ) THEN
            CALL EZPICK(COMMAND)
            CALL PX_GET_ALL_MENUS(ACTION_SUBMENU(MENU_INDX,IDX),
     &              NAME1,NAME2,ROUTINE,REMREC,NROUTINES,IER)
C
C ****  Select which menu name you wish to remove
C
            CALL DISPLAY_ITEMS
     &        (NROUTINES,NAME1,REMOVE_REM,'EDITING MENUS',ICOM)
            IF ( ICOM .NE. 0 ) THEN
C
C ****  Remove the VIEW from the RCP file of the menu item chosen
C
              CALL PX_DELETE_VIEW(COMMAND,NAME2(ICOM),IER)
C
C ****  Remove menu from common blocks
C
              DO J = 1, NROUTINES
                IF ( J .EQ. ICOM ) THEN
                  ACTION_BANK(MENU_INDX,IDX,J) =
     &                  ACTION_BANK(MENU_INDX,IDX,J+1)
                  ACTION_NAME(MENU_INDX,IDX,J) =
     &                  ACTION_NAME(MENU_INDX,IDX,J+1)
                  ACTION_ITEM(MENU_INDX,IDX,J) =
     &                  ACTION_ITEM(MENU_INDX,IDX,J+1)
                  ACTION_COMMAND(MENU_INDX,IDX,J) =
     &                  ACTION_COMMAND(MENU_INDX,IDX,J+1)
                ENDIF
              ENDDO
              ACTION_COUNT(MENU_INDX,IDX) =
     &              ACTION_COUNT(MENU_INDX,IDX) - 1
            ENDIF
            CALL EZRSET
C
C ****  CLEAR
C
          ELSEIF ( COMMAND2 .EQ. 'CLEAR' ) THEN
            I = 0
            J = 0
            CALL EZRNAM(COMMAND(1:CLEN),TEMP_RCP)
            CALL EZRNAM(ORIGINAL_RCP,COMMAND(1:CLEN))
            CALL PXBUILD_READ(COMMAND(1:CLEN))
C
C ****  ADD A NEW SCREEN to the list
C
          ELSEIF ( COMMAND2 .EQ. 'ADD' ) THEN
            CALL PXBUILD_ADD_NEW_ITEM(MENU_INDX,IDX)
          ENDIF
        ENDDO
        IF ( SUBMENU_COUNT(MENU_INDX) .EQ. 1 ) THEN
          ICOM = 0
        ENDIF
  800 ENDDO
C
C ****  Delete the temporary array with the original values
C
      CALL EZDROP(ORIGINAL_RCP)
      CALL STAMSG('          ',.TRUE.)
  999 RETURN
      END
