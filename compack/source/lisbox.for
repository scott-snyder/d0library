      SUBROUTINE LISBOX(TITLE,HELP,NSELECTED,SELECTED,FLAG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Display a dynamically created vertical
C-   list-box from which one or more items can be selected and return
C-   the id(s) of the selected item(s). Maximum of 1000 items.
C-
C-   Use LISFIL(0,' ') to initialize list-box and
C-       LISFIL(NITEM,ITEM) to fill it.
C-
C-   Inputs  : TITLE    [C*]  Title
C-             HELP     [C*]  Help Library
C-
C-   Outputs : NSELECTED    [I] Number of items selected
C-             SELECTED(*)  [I] Item(s) selected.
C-
C-   Controls: FLAG         [I] Control flag
C-                              Set to max number of items to be
C-                              selected per call to LISBOX.
C-
C-   Created  13-OCT-1991   Harrison B. Prosper
C-   Updated  23-OCT-1991   Harrison B. Prosper
C-   Updated  19-NOV-1991   Harrison B. Prosper
C-    ADD entry point LISHLP
C-   Updated  27-FEB-1992   Harrison B. Prosper
C-    Always clear DONE flags
C-   Updated   6-MAY-1992   Harrison B. Prosper
C-      Increase buffer size, add lisget
C-   Updated  19-JUN-1992   Harrison B. Prosper  
C-      Fix LISGET bug  
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) TITLE
      CHARACTER*(*) HELP
      INTEGER NSELECTED
      INTEGER SELECTED(*),FLAG
C
      INTEGER NITEM
      CHARACTER*(*) ITEM(*)
      INTEGER IX,IY,IR,IC,LINE
      INTEGER WAITIM,IDXDEFIN
      CHARACTER*(*) TOPIC,RECORD
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
      INCLUDE 'D0$INC:SMGCOM.INC'
      INCLUDE 'D0$INC:KEYCOM.INC'
C----------------------------------------------------------------------
      INTEGER DEF_MENU_ROW,DEF_MENU_COL,MENU_ROW,MENU_COL,ROWS,COLS
      INTEGER DEF_ROWS, DEF_COLS
      PARAMETER( DEF_MENU_ROW = 3)
      PARAMETER( DEF_MENU_COL = 3)
      PARAMETER( DEF_ROWS     =16)
      PARAMETER( DEF_COLS     =76)
      INTEGER MAXBUF
      PARAMETER( MAXBUF = 1000)
C----------------------------------------------------------------------
      INTEGER*2 KEY_CODE
      INTEGER TIMEOUT
      INTEGER TRULEN
      INTEGER STATUS,I,L,N,LTITLE,IDX,IDXDEF,II,JJ,NN,BUFLEN,LENSTR
      INTEGER LAST_NBUFFER,NBUFFER,VDID,LISTID,ROW,COL,IROW,ICOL
      CHARACTER*76 BUFFER(MAXBUF),CHOICE,HELP_TOPIC
      CHARACTER*10 OUTSTR
      LOGICAL FIRST,ACTIVE,OLD_EVENT_MODE,DONE(MAXBUF)
      LOGICAL EXIT,KEEP,REFRESH
      SAVE LAST_NBUFFER,VDID,FIRST,IDX,CHOICE,LISTID,BUFLEN
      SAVE KEEP,REFRESH
C----------------------------------------------------------------------
C&IF VAXVMS
      INTEGER SMG$CREATE_VIRTUAL_DISPLAY
      INTEGER SMG$CHANGE_VIRTUAL_DISPLAY
      INTEGER SMG$PASTE_VIRTUAL_DISPLAY
      INTEGER SMG$UNPASTE_VIRTUAL_DISPLAY
      INTEGER SMG$ERASE_DISPLAY
      INTEGER SMG$PUT_LINE
      INTEGER SMG$LABEL_BORDER
      INTEGER SMG$CREATE_MENU
      INTEGER SMG$DELETE_MENU
      INTEGER SMG$SELECT_FROM_MENU
      INTEGER SMG$SET_CURSOR_MODE
      INTEGER SMG$RETURN_CURSOR_POS
      INTEGER SMG$SET_KEYPAD_MODE
C----------------------------------------------------------------------
      INCLUDE '($SMGDEF)'
      INCLUDE '($TRMDEF)'
C----------------------------------------------------------------------
      DATA FIRST        /.TRUE./
      DATA BUFFER       /MAXBUF*' '/
      DATA MENU_ROW     /3/
      DATA MENU_COL     /3/
      DATA LAST_NBUFFER /0/
      DATA TIMEOUT      /0/
      DATA ROWS         /16/
      DATA COLS         /76/
      DATA IDXDEF       /1/
      DATA HELP_TOPIC   /' '/
      DATA KEEP         /.FALSE./
      DATA REFRESH      /.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
C
C ****  Create display for window
C
        STATUS = SMG$CREATE_VIRTUAL_DISPLAY(ROWS,COLS,VDID,SMG$M_BORDER)
        IF ( .NOT. STATUS ) THEN
          CALL MSGSCR(STATUS,'LISBOX')
        ENDIF
C
C ****  Create display for selected list
C
        STATUS = SMG$CREATE_VIRTUAL_DISPLAY
     &    (ROWS,COLS,LISTID,SMG$M_BORDER)
        IF ( .NOT. STATUS ) THEN
          CALL MSGSCR(STATUS,'LISBOX')
        ENDIF
      ENDIF
C
C ****  Clear DONE flags
C
      CALL VZERO(DONE,NBUFFER)
C
      IF ( REFRESH ) THEN
C
C ****  Clear remaining part of buffer
C
        IF ( NBUFFER .LT. LAST_NBUFFER ) THEN
          DO I =  NBUFFER+1, LAST_NBUFFER
            BUFFER(I) = ' '
          ENDDO
        ENDIF
C
        LAST_NBUFFER = NBUFFER
C
C ****  Check whether to read from file
C
        IF ( .NOT. TRMFLG ) THEN
          CALL RDLOG_BEGIN('LISBOX',&100)
          ACTIVE = .TRUE.
          NSELECTED = 0
          DO WHILE ( ACTIVE )
            OUTSTR = ' '
            CALL RDLOG('LISBOX',OUTSTR,LENSTR,&100)
            READ(OUTSTR,'(I10)') I
            NSELECTED = NSELECTED + 1
            SELECTED(NSELECTED) = I
          ENDDO
          GOTO 100
        ENDIF
C
C ****  Alter size of menu display
C
        LTITLE  = TRULEN(TITLE(1:LEN(TITLE)))
        ROW     = MIN(ROWS,NBUFFER,PBSAVE)
        COL     = MAX(LTITLE,BUFLEN)
        STATUS  = SMG$CHANGE_VIRTUAL_DISPLAY(VDID,ROW,COL+2)
        IF ( .NOT. STATUS ) THEN
          CALL MSGSCR(STATUS,'WINDIS')
        ENDIF
C
C ****  Change size of selected list
C
        IF ( FLAG .GT. 1 ) THEN
          IF ( ROW .GT. 12 ) THEN
            IROW = ROW - 5
          ELSE
            IROW = ROW
          ENDIF
          STATUS  = SMG$CHANGE_VIRTUAL_DISPLAY(LISTID,IROW,COL+2)
          IF ( .NOT. STATUS ) THEN
            CALL MSGSCR(STATUS,'LISBOX')
          ENDIF
          STATUS = SMG$ERASE_DISPLAY(LISTID)
          IF ( .NOT. STATUS ) THEN
            CALL MSGSCR(STATUS,'LISBOX')
          ENDIF
        ENDIF
C
C ****  Label menu display
C
        STATUS = SMG$LABEL_BORDER(VDID,TITLE(1:LTITLE))
        IF ( .NOT. STATUS ) THEN
          CALL MSGSCR(STATUS,'LISBOX')
        ENDIF
C
C ****  Create the menu
C
        STATUS = SMG$CREATE_MENU(VDID,BUFFER,SMG$K_VERTICAL)
        IF ( .NOT. STATUS ) THEN
          CALL MSGSCR(STATUS,'LISBOX')
        ENDIF
C
C ****  Paste menu display to pasteboard
C
        STATUS = SMG$PASTE_VIRTUAL_DISPLAY(VDID,PASTID,MENU_ROW,
     &    MENU_COL)
        IF ( .NOT. STATUS ) THEN
          CALL MSGSCR(STATUS,'LISBOX')
        ENDIF
      ENDIF
C
C ****  Cancel event mode and interrupt menu
C
      OLD_EVENT_MODE = EVENT_MODE
      CALL MENUEF(.FALSE.)
      ASTFLG = .FALSE.
C
      NSELECTED = 0
      EXIT   = .FALSE.
      ACTIVE = .TRUE.
      DO WHILE ( ACTIVE )
C
        STATUS = SMG$SET_CURSOR_MODE(PASTID,SMG$M_CURSOR_OFF)
        IF ( .NOT. STATUS ) THEN
          CALL MSGSCR(STATUS,'LISBOX')
        ENDIF
C
        IF ( TIMEOUT .GT. 0 ) THEN
          STATUS = SMG$SELECT_FROM_MENU(KEYID,
     &                                  VDID,
     &                                  IDX,IDXDEF,
     &                                  SMG$M_RETURN_IMMED,,
     &                                  TIMEOUT,
     &                                  KEY_CODE,
     &                                  CHOICE,,)
        ELSE
          STATUS = SMG$SELECT_FROM_MENU(KEYID,
     &                                  VDID,
     &                                  IDX,IDXDEF,
     &                                  SMG$M_RETURN_IMMED,,,
     &                                  KEY_CODE,
     &                                  CHOICE,,)
        ENDIF
C
        STATUS = SMG$SET_CURSOR_MODE(PASTID,SMG$M_CURSOR_ON)
        IF ( .NOT. STATUS ) THEN
          CALL MSGSCR(STATUS,'LISBOX')
        ENDIF
C
        STATUS = SMG$RETURN_CURSOR_POS(VDID,IROW,ICOL)
        IF ( .NOT. STATUS ) THEN
          CALL MSGSCR(STATUS,'LISBOX')
        ENDIF
C
        IDXDEF = IDX
C
C ****  NEXT SCREEN
C
        IF     ( (KEY_CODE .EQ. SMG$K_TRM_NEXT_SCREEN) .OR.
     &           (KEY_CODE .EQ. SMG$K_TRM_KP2) ) THEN
C
          IF     ( IROW .EQ. 1 ) THEN
            IDXDEF = MIN(NBUFFER,IDX + 2*ROW - 1)
          ELSEIF ( (NBUFFER-IDX) .LE. ROW ) THEN
            IDXDEF = NBUFFER
          ELSE
            IDXDEF = IDX + ROW
          ENDIF
C
C ****  PREVIOUS SCREEN
C
        ELSEIF ( (KEY_CODE .EQ. SMG$K_TRM_PREV_SCREEN) .OR.
     &           (KEY_CODE .EQ. SMG$K_TRM_KP8) ) THEN

          IF ( IDX .LE. ROW ) THEN
            IDXDEF = 1
          ELSE
            IDXDEF = IDX - ROW
          ENDIF
C
C ****  TOP-OF-PAGE
C
        ELSEIF ( (KEY_CODE .EQ. SMG$K_TRM_KP5) ) THEN
          IDXDEF = 1
C
C ****  BOTTOM-OF-PAGE
C
        ELSEIF ( (KEY_CODE .EQ. SMG$K_TRM_KP4) ) THEN
          IDXDEF = NBUFFER
C
C ****  HELP
C
        ELSEIF ( (KEY_CODE .EQ. SMG$K_TRM_HELP) .OR.
     &           (KEY_CODE .EQ. SMG$K_TRM_PF2) )   THEN
C
C ****  Display help from a help-library
C
          IF ( HELP_TOPIC(1:1) .NE. ' ' ) THEN
            I = INDEX(HELP_TOPIC,'/')
            IF ( I .GT. 0 ) THEN
              CHOICE = HELP_TOPIC(1:I-1)//' '//CHOICE
            ELSE
              CHOICE = HELP_TOPIC
            ENDIF
          ENDIF
          CALL HELPME(HELP(1:LEN(HELP)),CHOICE)
C
C ****  FIRST_UP (MOUSE BUTTON)
C
        ELSEIF (  KEY_CODE .EQ. SMG$K_TRM_FIRST_UP ) THEN
          ACTIVE = NSELECTED .LT. FLAG
C
        ELSEIF ( (KEY_CODE .EQ. SMG$K_TRM_SELECT)     .OR.
     &           (KEY_CODE .EQ. SMG$K_TRM_ENTER)      .OR.
     &           (KEY_CODE .EQ. 0)        .OR.  ! PF1 !!!
     &           (KEY_CODE .EQ. SMG$K_TRM_PF1)        .OR.
     &           (KEY_CODE .EQ. SMG$K_TRM_FIRST_DOWN) .OR.
     &           (KEY_CODE .EQ. SMG$K_TRM_CR)      ) THEN
C
C ****  Add selected id to list
C
          IF ( .NOT. DONE(IDX) ) THEN
            NSELECTED = NSELECTED + 1
            SELECTED(NSELECTED) = IDX
            DONE(IDX) = .TRUE.
C
C ****  Paste selected list display to pasteboard
C
            IF ( (FLAG .GT. 1) .AND. (NSELECTED .EQ. 1) ) THEN
              STATUS = SMG$PASTE_VIRTUAL_DISPLAY(LISTID,
     &                                           PASTID,
     &                                           MENU_ROW+4,
     &                                           MENU_COL+COL+1)
              IF ( .NOT. STATUS ) THEN
                CALL MSGSCR(STATUS,'LISBOX')
              ENDIF
            ENDIF
C
C ****  Write selected item to selection list.
C
            STATUS = SMG$PUT_LINE(LISTID,CHOICE)
            IF ( .NOT. STATUS ) THEN
              CALL MSGSCR(STATUS,'LISBOX')
            ENDIF
          ENDIF
C
          ACTIVE = (NSELECTED .LT. FLAG) .OR.
     &             (KEY_CODE  .EQ. SMG$K_TRM_FIRST_DOWN)
C
C ****  EXIT using mouse
C
        ELSEIF ( (KEY_CODE .EQ. SMG$K_TRM_SECOND_DOWN) .OR.
     &           (KEY_CODE .EQ. SMG$K_TRM_THIRD_DOWN) ) THEN
          EXIT = .TRUE.
        ELSEIF ( (KEY_CODE .EQ. SMG$K_TRM_SECOND_UP) .OR.
     &           (KEY_CODE .EQ. SMG$K_TRM_THIRD_UP) ) THEN
          IF ( EXIT ) THEN
            ACTIVE = .FALSE.
            KEEP   = .FALSE.
          ENDIF
        ELSE
C
C ****  EXIT
C
          ACTIVE  = .FALSE.
          KEEP    = .FALSE.
        ENDIF
      ENDDO
C
C ****  Unpaste menu display
C
      IF ( KEEP ) THEN
        REFRESH = .FALSE.
      ELSE
        REFRESH = .TRUE.
C
        STATUS = SMG$UNPASTE_VIRTUAL_DISPLAY(VDID,PASTID)
        IF ( .NOT. STATUS ) THEN
          CALL MSGSCR(STATUS,'LISBOX')
        ENDIF
C
C ****  Unpaste selected list display
C
        IF ( (FLAG .GT. 1) .AND. (NSELECTED .GT. 0) ) THEN
          STATUS = SMG$UNPASTE_VIRTUAL_DISPLAY(LISTID,PASTID)
          IF ( .NOT. STATUS ) THEN
            CALL MSGSCR(STATUS,'LISBOX')
          ENDIF
        ENDIF
C
C ****  Delete the menu
C
        STATUS = SMG$DELETE_MENU(VDID)
        IF ( .NOT. STATUS ) THEN
          CALL MSGSCR(STATUS,'LISBOX')
        ENDIF
      ENDIF
C
  100 CONTINUE
C
C ****  Log if LOGUP is TRUE
C
      IF ( LOGUP ) THEN
        CALL WTLOG_BEGIN('LISBOX')
        DO IDX = 1, NSELECTED
          WRITE(OUTSTR,'(I10)') SELECTED(IDX)
          CALL WTLOG(OUTSTR,LEN(OUTSTR),BUFFER(SELECTED(IDX)),COL+2)
        ENDDO
        CALL WTLOG_END('LISBOX')
      ENDIF
C
C&ELSE
C&
C&ENDIF
      MENU_ROW = DEF_MENU_ROW
      MENU_COL = DEF_MENU_COL
      ROWS     = DEF_ROWS
      COLS     = DEF_COLS
      TIMEOUT  = 0
      IDXDEF   = 1
      CALL MENUEF(OLD_EVENT_MODE)
      HELP_TOPIC = ' '
C
C ****  Load last item into CHOICE string
C
      IF ( NSELECTED .GT. 0 ) THEN
        CHOICE = BUFFER(SELECTED(NSELECTED))
      ELSE
        CHOICE = ' '
      ENDIF
      RETURN
C
      ENTRY LISPOS(IX,IY)
      MENU_ROW = IX
      MENU_COL = IY
      RETURN
C
      ENTRY LISSIZ(IR)
      ROWS = IR
      RETURN
C
      ENTRY LISDEF(IDXDEFIN)
      IDXDEF = IDXDEFIN
      RETURN
C
      ENTRY LISTIM(WAITIM)
      TIMEOUT = WAITIM
      RETURN
C
      ENTRY LISGET(RECORD)
      RECORD = CHOICE
      RETURN
C
      ENTRY LISKEEP
      KEEP = .TRUE.
      RETURN
C
      ENTRY LISFIL(NITEM,ITEM)
C
C ****  Load local buffer
C
      IF ( NITEM .LE. 0 ) THEN
        NBUFFER = 0
      ELSE
C
        N = MIN(MAXBUF,NITEM)
        BUFLEN = LEN(ITEM(1))
C
        I = 0
        DO WHILE ( I .LT. N )
          I = I + 1
          IF ( NBUFFER .LT. MAXBUF ) THEN
            CALL WORD(ITEM(I),II,JJ,NN)
            IF ( NN .GT. 0 ) THEN
              NBUFFER = NBUFFER + 1
              BUFFER(NBUFFER) = ITEM(I)
              DONE(NBUFFER)   = .FALSE.
            ENDIF
          ELSE
            CALL INTMSG(' ** Maximum buffer size exceeded in LISFIL')
            I = N
          ENDIF
        ENDDO
      ENDIF
      RETURN
C
C ****  Entry point to specify help topic prior to a call to LISBOX.
C
      ENTRY LISHLP(TOPIC)
      HELP_TOPIC = TOPIC
  999 RETURN
      END
