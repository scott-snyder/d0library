      SUBROUTINE PXBUILD_READ(BANK_NAME)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read Display Description File and set up
C-   menus for selection of action routines.
C-
C-   Inputs  : BAN_NAME [C*]: Nane of the bank to be read.  If the bank name is
C-                            blank the routine will ask for a file name to the
C-                            user.
C-
C-   Outputs : None
C-
C-   Created  12-SEP-1990   Harrison B. Prosper
C-   Updated   9-NOV-1990   Harrison B. Prosper
C-   Updated   7-MAY-1991   Lupe Howell  Reading submenus arrays
C-   Updated  19-JUN-1991   Lupe Howell  Clearing description file name
C-                          if the file was not found
C-   Updated   2-DEC-1991   Lupe Howell Reading information from PX_*_RCP
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) BANK_NAME
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:PXBUILDCOM.INC'
      INCLUDE 'D0$OFFLINE_UTIL$GENERAL:VALUE.DEF'
C----------------------------------------------------------------------
      CHARACTER*80 STRING,AREA,DDFILE,FILENAME
      CHARACTER*32 NAME,RCPBANK
C
      INTEGER MAXROUTINES
      PARAMETER( MAXROUTINES = 200 )
      CHARACTER*32 ROUTINE(1:MAXROUTINES)
      CHARACTER*32 ACTION(MAXROUTINES)
      CHARACTER*32 SUBMENU_NAME,NAME1(MAXROUTINES),NAME2(MAXROUTINES)
      CHARACTER*32 TEMP_ACTION(MAXLIST),OLD_RCP
      INTEGER REMREC(MAXROUTINES)
C
      INTEGER I,II,J,N,IER,JJ,K
      INTEGER CONTROL, NROUTINES, LENGTH, LAREA, PFNUM
      INTEGER TOTAL_MENUS,BLEN
C
      LOGICAL LAST, EZERROR, NEW, FULL,ACTIVE,RCP_MATCH
C----------------------------------------------------------------------
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      SAVE FIRST
C----------------------------------------------------------------------
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL EZPICK('PXBUILD_RCP')
        CALL EZGETS('DISPLAY_FILE_AREA',1,AREA,LAREA,IER)
        CALL EZRSET
        IF ( IER .NE. 0 ) THEN
          AREA = ' '
          LAREA= 0
        ENDIF
        CALL MENNEW('RCP_FILES')
      ENDIF
C
C ****  Get list of Description files.  
C
      IF ( BANK_NAME .EQ. ' ' ) THEN
        FILENAME = ' '
        STRING = ' '
        CALL OUTMSG('1')
        CALL OUTMSG(' Name one or more PIXIE Action List Files')
        CALL GETPAR(1,' Enter name(s)> ','U',FILENAME)
      ELSE
C
C ****  If there was a bank name input into the routine
C ****  get the file name out of it
C
        CONTROL = -1                      ! Trim tokens
        LAST    = .FALSE.
        CALL GET_NEXT_TOKEN(BANK_NAME,'RCP',I,J,N,LAST,CONTROL)
        FILENAME = BANK_NAME(I:J-1)
      ENDIF
      IF ( PFNUM() .EQ. 4 ) GOTO 999
C
C *******************************************************
C ****  Read files into memory and add to menu
C *******************************************************
C
      CONTROL = -1                      ! Trim tokens
      LAST    = .FALSE.
      DO WHILE ( .NOT. LAST )
C
C ****  Get file Name from string
C
        CALL GET_NEXT_TOKEN(FILENAME,',',I,J,N,LAST,CONTROL)
        NAME = FILENAME(I:J)
C
C ****  Create RCP bank name etc.
C
        I = INDEX(NAME,'.RCP')
        IF ( I .GT. 0 ) THEN
          N = I - 1
        ENDIF
        NAME    = NAME(1:N)
        DDFILE  = AREA(1:LAREA)//NAME(1:N)//'.RCP'
        RCPBANK = NAME(1:N)//'_RCP'
C
C ****  Check if we already have this file
C
        NEW = .TRUE.
        IF ( MENU_COUNT .GT. 0 ) THEN
          I = 0
          DO WHILE ( NEW .AND. (I .LT. MENU_COUNT) )
            I = I + 1
            IF ( MENU_NAME(I) .EQ. NAME(1:32) ) THEN
              NEW = .FALSE.
              MENU_COUNT = I
            ENDIF
          ENDDO
        ENDIF
C
C ****  If the file read is new increment the menu count
C
        FULL = .FALSE.
        IF ( NEW ) THEN
          IF ( MENU_COUNT .LT. MAXMENU ) THEN
            MENU_COUNT = MENU_COUNT + 1
            MENU_NAME(MENU_COUNT) = NAME(1:N)
          ELSE
            FULL = .TRUE.
          ENDIF
        ENDIF
C
C ****  Read RCP File
C
        IF ( .NOT. FULL ) THEN
          CALL INTMSG(' Reading file : '//DDFILE)
          IF ( NEW ) THEN
            CALL INRCP(DDFILE,IER)
          ENDIF
          IF ( IER .EQ. 0 ) THEN
            CALL INTMSG(' Done!')
C
C ****  Reading Submenu array
C
            CALL EZPICK(RCPBANK)
            IF ( EZERROR(IER) ) THEN
              CALL INTMSG(' Could NOT find bank '//RCPBANK)
              GOTO 999
            ENDIF
            CALL EZ_GET_CHARS('COMPACK_MENUS',
     &           SUBMENU_COUNT(MENU_COUNT),TEMP_ACTION,IER)
            DO JJ = 1, SUBMENU_COUNT(MENU_COUNT)
              ACTION_SUBMENU(MENU_COUNT,JJ) = TEMP_ACTION(JJ)
            ENDDO
            CALL EZRSET
            IF ( IER .EQ. 0 ) THEN
              IF ( NEW ) THEN
                CALL MENNEW(NAME)
              ENDIF
              DO I=1, SUBMENU_COUNT(MENU_COUNT)
C
C ****  Add submenus items to actions names if there is more than one
C ****  submenu
C
                IF ( SUBMENU_COUNT(MENU_COUNT) .GT. 1 ) THEN
C
C ****  Now build lower menu for each submenu
C
                  SUBMENU_NAME = ACTION_SUBMENU(MENU_COUNT,I)
                ELSE
                  SUBMENU_NAME = NAME
                ENDIF
                CALL EZPICK(RCPBANK)
                IF ( EZERROR(IER) ) THEN
                  CALL INTMSG(' Could NOT find Bank '//RCPBANK)
                ELSE
C
C ****  Get list of Action Routines
C
                  CALL PX_GET_ALL_MENUS(ACTION_SUBMENU(MENU_COUNT,I),
     &              NAME1,NAME2,ACTION,REMREC,TOTAL_MENUS,IER)
                  IF ( IER .NE. 0 ) THEN
                    CALL INTMSG(' Could NOT find variables for '//
     &                    ACTION(II))
                  ELSE
                    DO II = 1, TOTAL_MENUS
C
C ****  Add menu items to common blocks
C
                      ACTION_COUNT(MENU_COUNT,I) =
     &                  ACTION_COUNT(MENU_COUNT,I) + 1
C
C ****  Action routine name
C
                      ACTION_NAME
     &                  (MENU_COUNT,I,ACTION_COUNT(MENU_COUNT,I)) =
     &                  ACTION(II)
C
C ****  Action bank
C
                      ACTION_BANK
     &                  (MENU_COUNT,I,ACTION_COUNT(MENU_COUNT,I)) =
     &                  RCPBANK
C
C ****  First menu item "Action_Item"
C
                      ACTION_ITEM
     &                  (MENU_COUNT,I,ACTION_COUNT(MENU_COUNT,I)) =
     &                  NAME1(II)
C
C ****  Second menu item "Action_Command"
C
                      ACTION_COMMAND
     &                  (MENU_COUNT,I,ACTION_COUNT(MENU_COUNT,I)) =
     &                  NAME2(II)
                    ENDDO
                  ENDIF
                ENDIF
                CALL EZRSET
              ENDDO
C
C ****  Store RCP file name(s)
C
              RCP_MATCH = .FALSE.
              CALL WORD(RCPBANK,K,J,BLEN)
              K = 0
              DO WHILE ( K .LT. RCPFILE_COUNT )
                K = K + 1
                IF ( RCPBANK(1:BLEN) .EQ.
     &              RCPFILE_NAME(K)(1:BLEN) ) THEN
                  RCP_MATCH = .TRUE.
                ENDIF
              ENDDO
              IF ( .NOT. RCP_MATCH ) THEN
                RCPFILE_COUNT = RCPFILE_COUNT + 1
                RCPFILE_NAME(RCPFILE_COUNT) = RCPBANK
                RCPFILE_INDX(RCPFILE_COUNT) = MENU_COUNT
                OLD_RCP = RCPFILE_NAME(RCPFILE_COUNT)
                CALL MENADD('RCP_FILES',.TRUE.,RCPBANK,RCPBANK,
     &           'Modify RCP file '//RCPBANK)
                CALL PXBUILD_EDIT_MENU(RCPBANK)
              ENDIF
            ELSE
              CALL INTMSG(' Error reading SUBMENU_ROUTINES')
            ENDIF
          ELSE
            CALL INTMSG(' Error reading file')
            IF ( NEW ) THEN
              MENU_NAME(MENU_COUNT) = ' '
              MENU_COUNT = MENU_COUNT - 1
            ENDIF
          ENDIF
        ELSE
          CALL INTMSG(' No more items can be added: buffer full')
        ENDIF
      ENDDO

  999 RETURN
      END
