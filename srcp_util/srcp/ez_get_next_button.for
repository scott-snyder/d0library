      SUBROUTINE EZ_GET_NEXT_BUTTON (MENU,ADD_MENU_ITEM,SWITCH,
     &  ITEM,LITEM,
     &  COMMAND,LCOMMAND,
     &  ACTION,LACTION,
     &  HELP,LHELP,
     &  LAST,CONTROL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return the next (ADD,ITEM,COMMAND,ACTION,HELP)
C-   quintet from the given COMPACK menu parameter. Note: CONTROL
C-   must be a variable.
C-
C-   Inputs  : MENU             [C*]    Name of COMPACK menu
C-
C-   Outputs : ADD_MENU_ITEM    [L]     Flag indicating if item is active
C-             SWITCH           [L]     Arguments of MENADD
C-             ITEM             [C*]    Button name
C-             LITEM            [I]     Length of button name
C-             COMMAND          [C*]    Name of associated command
C-             LCOMMAND         [I]     Length of command name
C-             ACTION           [C*]    Name of associated action
C-             LACTION          [I]     Length of action name
C-             HELP             [C*]    Associated help text
C-             LHELP            [I]     Length of help text
C-             LAST             [L]     True if last button (item)
C-
C-   Controls: CONTROL          [I]     Set to zero upon first call
C-                                      It is set to one upon return
C-
C-   Created  29-MAY-1990   Harrison B. Prosper
C-   Updated   3-JUL-1991   Harrison B. Prosper
C-      Add ACTION
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      CHARACTER*(*) MENU
      LOGICAL ADD_MENU_ITEM
      LOGICAL SWITCH
      CHARACTER*(*) ITEM
      INTEGER LITEM
      CHARACTER*(*) COMMAND
      INTEGER LCOMMAND
      CHARACTER*(*) ACTION
      INTEGER LACTION
      CHARACTER*(*) HELP
      INTEGER LHELP
      LOGICAL LAST
      INTEGER CONTROL
C----------------------------------------------------------------------
      INTEGER IER
C
      INTEGER I,PTR,SAVED_PTR,ITYPE,IVAL
      INTEGER LLHELP,LENGTH,MAXLHELP
C
      CHARACTER*132 STRING
C
      LOGICAL EZERROR,ACTIVE
C----------------------------------------------------------------------
      INCLUDE 'D0$OFFLINE_UTIL$GENERAL:VALUE.DEF'
C----------------------------------------------------------------------
      SAVE PTR
C----------------------------------------------------------------------
      IF ( CONTROL .LE. 0 ) THEN
        PTR     = 1
        CONTROL = 1
        LAST    = .FALSE.
        HELP    = ' '
      ENDIF
C
C ****  Get ADD_MENU_ITEM flag
C
      CALL EZGET_NEXT_VALUE_TYPE
     &            (MENU,ADD_MENU_ITEM,STRING,ITYPE,LENGTH,IER,PTR)
C
      IF ( IER .NE. 0 ) THEN
        LAST = .TRUE.
        GOTO 999
      ELSE
C
C ****  Get MENADD arguments
C
        CALL EZGET_NEXT_VALUE_TYPE
     &            (MENU,SWITCH,STRING,ITYPE,LENGTH,IER,PTR)
C
C ****  Get MENU item
C
        CALL EZGET_NEXT_VALUE_TYPE
     &            (MENU,IVAL,ITEM,ITYPE,LITEM,IER,PTR)
C
C ****  Get MENU command
C
        CALL EZGET_NEXT_VALUE_TYPE
     &            (MENU,IVAL,COMMAND,ITYPE,LCOMMAND,IER,PTR)
        SAVED_PTR = PTR
C
C ****  Get ACTION name (identified by a % prefix)
C
        CALL EZGET_NEXT_VALUE_TYPE
     &            (MENU,IVAL,ACTION,ITYPE,LACTION,IER,PTR)
        IF ( ACTION(1:1) .EQ. '%' ) THEN
          ACTION = ACTION(2:LACTION)
          LACTION= LACTION - 1
        ELSE
          ACTION = ' '                  ! No action present
          LACTION= 0
          PTR = SAVED_PTR               ! Restore pointer
        ENDIF
C
C ****  Get HELP for this item
C
        MAXLHELP = LEN(HELP)
        LHELP    = 1
        ACTIVE   = .TRUE.
        DO WHILE ( ACTIVE )
C
          CALL EZGET_NEXT_VALUE_TYPE
     &              (MENU,IVAL,STRING,ITYPE,LENGTH,IER,PTR)
C
          IF ( ITYPE .GT. VTCHR ) THEN
            LLHELP = LHELP + 1 + LENGTH
            IF ( LLHELP .LE. MAXLHELP ) THEN
              HELP = HELP(1:LHELP)//' '//STRING(1:LENGTH)
              LHELP= LLHELP
            ENDIF
            IF ( IER .NE. 0 ) THEN        ! Check for LAST value
              LAST = .TRUE.
              ACTIVE = .FALSE.
            ENDIF
          ELSE
            IF ( IER .EQ. 0 ) THEN
              PTR = PTR - 1         ! BACKSPACE
            ENDIF
            ACTIVE = .FALSE.
          ENDIF
        ENDDO
      ENDIF
C
  999 RETURN
      END
