      SUBROUTINE EZ_SETUP_COMPACK (RCP_FILE,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Setup COMPACK via an RCP file. See the note
C-   D0$SRCP_UTIL:EZ_SETUP_COMPACK.DOC.
C-
C-   Inputs  : RCP_FILE [C*]    Name of RCP-file
C-   Outputs : IER      [I]     0 --- Ok.
C-   Controls:
C-
C-   Created  29-MAY-1990   Harrison B. Prosper
C-   Updated  22-DEC-1990   Harrison B. Prosper
C-      Added check on ACTIVE flag
C-   Updated  15-MAY-1991   Harrison B. Prosper
C-      Add more COMPACK init
C-   Updated  27-JUN-1991   Harrison B. Prosper
C-      Add call to EZ_GET_MENUS_TITLES
C-   Updated   3-JUL-1991   Harrison B. Prosper
C-      Change ez_get_next_button calling sequence
C-   Updated  20-OCT-1991   Harrison B. Prosper
C-    Add KEYDEF
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) RCP_FILE
      INTEGER IER
C----------------------------------------------------------------------
      INCLUDE 'D0$OFFLINE_UTIL$GENERAL:VALUE.DEF'
C----------------------------------------------------------------------
      INTEGER I,J,N,PTR,IIER
      INTEGER NUMBER_LINES
      INTEGER NUMBER_MENUS
      INTEGER LHELP,LLHELP,LITEM,LCOMMAND,LACTION,LENGTH,CONTROL
C
      INTEGER MAXMENU
      PARAMETER( MAXMENU = 50 )
      CHARACTER*32 MENU(MAXMENU),ACTION
C
      CHARACTER*40 ITEM,COMMAND
      CHARACTER*132 STRING,WELCOME,TITLE(MAXMENU)
C
      CHARACTER*1600 HELP
C
C ****  KeyPad Definition
C
      INTEGER NKEY_NAME,NKEYNAM
      PARAMETER( NKEY_NAME = 18 )
      INTEGER KEY_NUMBER(NKEY_NAME),KEYNUM(NKEY_NAME)
      CHARACTER*16 KEY_NAME(NKEY_NAME)
      CHARACTER*32 KEY(2,NKEY_NAME),KEYNAM(NKEY_NAME)
C
      LOGICAL EZERROR,SWITCH,CREATE_WINDOW,LAST,CALL_MENDEF,FOUND
      LOGICAL ADD_SYSTEM_ITEM,ADD_MENU_ITEM,ACTIVE,SETUP_KEYPAD
C----------------------------------------------------------------------
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST/.TRUE./
      DATA SETUP_KEYPAD/.TRUE./
      DATA KEY_NUMBER/0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17/
      DATA KEY_NAME/'KP0',
     &              'KP1','KP2','KP3',
     &              'KP4','KP5','KP6',
     &              'KP7','KP8','KP9',
     &              'PF1','PF2','PF3','PF4',
     &              'MINUS','COMMA','ENTER','PERIOD'/
C----------------------------------------------------------------------
C
C ****  Select RCP bank
C
      CALL EZPICK (RCP_FILE(1:LEN(RCP_FILE)))
      IF ( EZERROR(IER) ) THEN
        GOTO 999
      ENDIF
C
C ****  Check active flag if present
C
      CALL EZGET('ACTIVE',ACTIVE,IER)
C
      IF ( IER .EQ. 0 ) THEN
        IF ( .NOT. ACTIVE ) THEN
          CALL ERRMSG('NOT_ACTIVE','EZ_SETUP_COMPACK',
     &      ' Menu NOT set up: ACTIVE switch is FALSE','S')
          CALL EZRSET
          GOTO 999
        ENDIF
      ENDIF
C
C ****  Get Menu list
C
      CALL EZ_GET_MENUS_TITLES(MENU,TITLE,NUMBER_MENUS,IER)
      IF ( EZERROR(IER) ) THEN
        CALL ERRMSG('NO_MENUS','EZ_SETUP_COMPACK',
     &      ' No menus found; Menus NOT set up','S')
        CALL EZRSET
        GOTO 900
      ENDIF
C
C ****  Loop over MENUs
C
      HELP = ' '
      DO I =  1, NUMBER_MENUS
        CALL MENNEW(MENU(I))            ! Define NEW menu
C
        CONTROL = 0
        LAST    = .FALSE.
        DO WHILE ( .NOT. LAST )
          CALL EZ_GET_NEXT_BUTTON(MENU(I),
     &                            ADD_MENU_ITEM,
     &                            SWITCH,
     &                            ITEM,LITEM,
     &                            COMMAND,LCOMMAND,
     &                            ACTION,LACTION,
     &                            HELP,LHELP,
     &                            LAST,
     &                            CONTROL)
C
C ****  Add menu item
C
          IF ( ADD_MENU_ITEM ) THEN
            CALL MENADD(MENU(I),SWITCH,
     &                  ITEM(1:LITEM),
     &                  COMMAND(1:LCOMMAND),
     &                  HELP(1:LHELP))
          ENDIF
        ENDDO
      ENDDO
C
  900 CONTINUE
      IER = 0                           ! Reset error code
C
C ****  Call MENDEF to define system level commands
C
      IF ( FIRST ) THEN
        CALL EZGET('MENDEF',CALL_MENDEF,IIER)
        IF ( .NOT. EZERROR(IIER)  ) THEN
          IF ( CALL_MENDEF ) THEN
            CALL MENDEF                       ! COMPACK's menu control
          ENDIF
        ENDIF
        FIRST = .FALSE.
      ENDIF
C
C ****  Setup KEYPAD
C
      IF ( SETUP_KEYPAD ) THEN
C
        CALL EZ_GET_CHARS('KEYDEF',N,KEY,IIER)
C
        IF ( IIER .EQ. 0 ) THEN
          CALL SRTCHR(KEY_NAME,NKEY_NAME,KEY_NUMBER)
          SETUP_KEYPAD = .FALSE.
          NKEYNAM = 0
C
          N = N/2
          DO I =  1, N
            CALL LOCSTR(KEY(1,I),KEY_NAME,NKEY_NAME,FOUND,J)
            IF ( FOUND ) THEN
              NKEYNAM = NKEYNAM + 1
              KEYNUM(NKEYNAM) = KEY_NUMBER(J)
              KEYNAM(NKEYNAM) = KEY(2,I)
            ENDIF
          ENDDO
          IF ( NKEYNAM .GT. 0 ) THEN
            CALL KEYDEF(NKEYNAM,KEYNUM,KEYNAM)
          ENDIF
        ENDIF
      ENDIF
C
C ****  Setup COMPACK windows
C
      CALL EZGET('SETSTA',NUMBER_LINES,IIER)
      IF ( .NOT. EZERROR(IIER) ) THEN
        CALL SETSTA (NUMBER_LINES)
      ENDIF
C
      CALL EZGET('SETLIN',NUMBER_LINES,IIER)
      IF ( .NOT. EZERROR(IIER) ) THEN
        CALL SET_SPLIT_SCREEN (NUMBER_LINES)
      ENDIF
C
      CALL EZGET('DEFSPA',NUMBER_LINES,IIER)
      IF ( .NOT. EZERROR(IIER) ) THEN
        CALL DEFSPA (NUMBER_LINES)
      ENDIF
C
      CALL EZGET('DEFCOL',NUMBER_LINES,IIER)
      IF ( .NOT. EZERROR(IIER) ) THEN
        CALL DEFCOL (NUMBER_LINES)
      ENDIF
C
      CALL EZGET('SPLSTA',CREATE_WINDOW,IIER)
      IF ( CREATE_WINDOW ) THEN
        CALL SPLSTA
      ENDIF
C
      CALL EZGET('SPLTIT',CREATE_WINDOW,IIER)
      IF ( CREATE_WINDOW ) THEN
        CALL SPLTIT                       ! Set split screen mode
      ENDIF
C
      CALL EZGETS('WELCOME',1,WELCOME,LENGTH,IIER)
      IF ( LENGTH .GT. 0 ) THEN
        CALL STAMSG(' '//WELCOME(1:LENGTH),.TRUE.)
      ENDIF
C
      CALL EZRSET
C
  999 RETURN
      END
