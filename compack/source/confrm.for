      FUNCTION CONFRM(TITLE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Display a dynamically created confirmation
C-   box. Use the first mouse button to select.
C-
C-   Inputs  : TITLE  [C*]  Title of Confirm Box
C-   Outputs :
C-   Controls:
C-
C-   Created  13-OCT-1991   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) TITLE
      INTEGER IX,IY
C----------------------------------------------------------------------
      LOGICAL CONFRM,CONPOS
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
      INCLUDE 'D0$INC:SMGCOM.INC'
      INCLUDE 'D0$INC:KEYCOM.INC'
C----------------------------------------------------------------------
      INTEGER DEF_MENU_ROW,DEF_MENU_COL,MENU_ROW,MENU_COL
      INTEGER DEF_ROWS, DEF_COLS, ROWS,COLS
      PARAMETER( DEF_MENU_ROW =  3)
      PARAMETER( DEF_MENU_COL = 32)
      PARAMETER( DEF_ROWS     =  2)
      PARAMETER( DEF_COLS     = 16)
      INTEGER SELECTED
      LOGICAL OLD_EVENT_MODE
C----------------------------------------------------------------------
C&IF VAXVMS
      INTEGER SMG$CREATE_VIRTUAL_DISPLAY
      INTEGER SMG$PASTE_VIRTUAL_DISPLAY
      INTEGER SMG$UNPASTE_VIRTUAL_DISPLAY
      INTEGER SMG$CHANGE_VIRTUAL_DISPLAY
      INTEGER SMG$LABEL_BORDER
      INTEGER SMG$CREATE_MENU
      INTEGER SMG$DELETE_MENU
      INTEGER SMG$SELECT_FROM_MENU
      INTEGER SMG$SET_CURSOR_MODE
C----------------------------------------------------------------------
      INCLUDE '($SMGDEF)'
      INCLUDE '($TRMDEF)'
C----------------------------------------------------------------------
      INTEGER*2 KEY_CODE
      INTEGER STATUS,I,L,N,IDX,IDXDEF,BUFLEN,LTITLE,TRULEN
      INTEGER VDID
      LOGICAL ACTIVE,EXIT
      CHARACTER*12 ITEM(2),CHOICE
C----------------------------------------------------------------------
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST    /.TRUE./
      DATA MENU_ROW /3/
      DATA MENU_COL /32/
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        ITEM(1) = 'OK'
        ITEM(2) = 'CANCEL'
        ROWS    = 2
        COLS    = LEN(ITEM(1))
C
C ****  Create display for window
C
        STATUS = SMG$CREATE_VIRTUAL_DISPLAY(ROWS,COLS,VDID,SMG$M_BORDER)
        IF ( .NOT. STATUS ) THEN
          CALL MSGSCR(STATUS,'CONFRM')
        ENDIF
      ENDIF
C
C ****  Label menu display
C
      LTITLE = TRULEN(TITLE(1:LEN(TITLE)))
      LTITLE = MIN(LTITLE,COLS)
      STATUS = SMG$LABEL_BORDER(VDID,
     &                          TITLE(1:LTITLE),,,
     &                          SMG$M_BOLD)
      IF ( .NOT. STATUS ) THEN
        CALL MSGSCR(STATUS,'CONFRM')
      ENDIF
C
      STATUS = SMG$SET_CURSOR_MODE(PASTID,
     &                             SMG$M_CURSOR_OFF)
      IF ( .NOT. STATUS ) THEN
        CALL MSGSCR(STATUS,'CONFRM')
      ENDIF
C
C ****  Create the menu
C
      STATUS = SMG$CREATE_MENU(VDID,ITEM,SMG$K_VERTICAL)
      IF ( .NOT. STATUS ) THEN
        CALL MSGSCR(STATUS,'CONFRM')
      ENDIF
C
C ****  Paste menu display to pasteboard
C
      STATUS = SMG$PASTE_VIRTUAL_DISPLAY(VDID,PASTID,MENU_ROW,MENU_COL)
      IF ( .NOT. STATUS ) THEN
        CALL MSGSCR(STATUS,'CONFRM')
      ENDIF
C
C ****  Cancel event mode
C
      OLD_EVENT_MODE = EVENT_MODE
      IF ( OLD_EVENT_MODE ) THEN
        CALL MENUEF(.FALSE.)
      ENDIF
C
      SELECTED = 0
      IDXDEF   = 1
      ACTIVE = .TRUE.
      DO WHILE ( ACTIVE )
        STATUS = SMG$SELECT_FROM_MENU(KEYID,
     &                                VDID,
     &                                IDX,IDXDEF,
     &                                SMG$M_RETURN_IMMED,,,
     &                                KEY_CODE,
     &                                CHOICE,,)
C
        IF (  KEY_CODE .EQ. SMG$K_TRM_FIRST_DOWN  ) THEN
          SELECTED = IDX
          IDXDEF   = IDX
          EXIT     = .TRUE.
C
        ELSEIF ( (KEY_CODE .EQ. SMG$K_TRM_SELECT)     .OR.
     &           (KEY_CODE .EQ. SMG$K_TRM_ENTER)      .OR.
     &           (KEY_CODE .EQ. 0)        .OR.  ! PF1 !!!
     &           (KEY_CODE .EQ. SMG$K_TRM_PF1)        .OR.
     &           (KEY_CODE .EQ. SMG$K_TRM_CR)      ) THEN
          SELECTED = IDX
          ACTIVE = .FALSE.
C
        ELSEIF ( (KEY_CODE .EQ. SMG$K_TRM_SECOND_DOWN)  .OR.
     &           (KEY_CODE .EQ. SMG$K_TRM_THIRD_DOWN) ) THEN
          EXIT = .TRUE.
        ELSEIF ( (KEY_CODE .EQ. SMG$K_TRM_FIRST_UP)   .OR.
     &           (KEY_CODE .EQ. SMG$K_TRM_SECOND_UP)  .OR.
     &           (KEY_CODE .EQ. SMG$K_TRM_THIRD_UP) ) THEN
          ACTIVE = .NOT. EXIT
        ENDIF
      ENDDO
C
C ****  Unpaste menu display
C
      STATUS = SMG$UNPASTE_VIRTUAL_DISPLAY(VDID,PASTID)
      IF ( .NOT. STATUS ) THEN
        CALL MSGSCR(STATUS,'UNPASTE_VIRTUAL_DISPLAY')
      ENDIF
C
      STATUS = SMG$SET_CURSOR_MODE(PASTID,
     &                             SMG$M_CURSOR_ON)
      IF ( .NOT. STATUS ) THEN
        CALL MSGSCR(STATUS,'SET_CURSOR_MODE')
      ENDIF
C
C ****  Delete the menu
C
      STATUS = SMG$DELETE_MENU(VDID)
      IF ( .NOT. STATUS ) THEN
        CALL MSGSCR(STATUS,'CONFRM')
      ENDIF
C
C&ELSE
C&
C&ENDIF
      MENU_ROW = DEF_MENU_ROW
      MENU_COL = DEF_MENU_COL
      CALL MENUEF(OLD_EVENT_MODE)
      CONFRM = SELECTED .EQ. 1
      RETURN
C
      ENTRY CONPOS(IX,IY)
      MENU_ROW = IX
      MENU_COL = IY
  999 RETURN
      END
