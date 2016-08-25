      SUBROUTINE WINDIS(TITLE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Display a dynamically created scrollable
C-   window. Maximum of 500 lines.
C-
C-   Inputs  : TITLE    [C*]  Title
C-   Outputs :
C-   Controls:
C-
C-   Created  13-OCT-1991   Harrison B. Prosper
C-   Updated   7-MAY-1992   Harrison B. Prosper  
C-      Set ASTFLG = .FALSE. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) TITLE
      INTEGER NITEM
      CHARACTER*(*) ITEM(*)
      INTEGER IX,IY,IR,IC,LINE
      CHARACTER*(*) OUTSTR
      INTEGER WAITIM
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
      INCLUDE 'D0$INC:SMGCOM.INC'
      INCLUDE 'D0$INC:KEYCOM.INC'
C----------------------------------------------------------------------
      INTEGER DEF_VIEW_ROW,DEF_VIEW_COL,VIEW_ROW,VIEW_COL,ROWS,COLS
      INTEGER DEF_ROWS, DEF_COLS
      PARAMETER( DEF_VIEW_ROW = 4)
      PARAMETER( DEF_VIEW_COL = 2)
      PARAMETER( DEF_ROWS     =16)
      PARAMETER( DEF_COLS     =78)
      INTEGER MAXROWS, MAXCOLS
      PARAMETER( MAXROWS = 500)
      PARAMETER( MAXCOLS = 132)
C----------------------------------------------------------------------
      INTEGER LIBKEY
      INTEGER KEY_CODE
      INTEGER TRULEN
      INTEGER STATUS,I,L,N,LTITLE,NEWLINE,NEWCOL,II,JJ,NN
      INTEGER NBUFFER,VDID,ROW,COL,IROW,ICOL,TIMEOUT
      LOGICAL FIRST,ACTIVE,OLD_EVENT_MODE
      SAVE NBUFFER,VDID,FIRST,NEWLINE,NEWCOL
C----------------------------------------------------------------------
C&IF VAXVMS
      INCLUDE '($SMGDEF)'
      INCLUDE '($TRMDEF)'
C&ELSE
C&      INCLUDE 'D0$INC:SMGDEF.DEF'
C&      INCLUDE 'D0$INC:TRMDEF.DEF'
C&ENDIF
C----------------------------------------------------------------------
      INTEGER SMG$CREATE_VIRTUAL_DISPLAY
      INTEGER SMG$PUT_LINE
C&IF VAXVMS
      INTEGER SMG$CREATE_VIEWPORT
      INTEGER SMG$DELETE_VIEWPORT
      INTEGER SMG$CHANGE_VIEWPORT
      INTEGER SMG$CHANGE_VIRTUAL_DISPLAY
      INTEGER SMG$PASTE_VIRTUAL_DISPLAY
      INTEGER SMG$UNPASTE_VIRTUAL_DISPLAY
      INTEGER SMG$LABEL_BORDER
      INTEGER SMG$SET_CURSOR_MODE
C----------------------------------------------------------------------
      DATA FIRST        /.TRUE./
      DATA VIEW_ROW     /4/
      DATA VIEW_COL     /2/
      DATA TIMEOUT      /0/
      DATA ROWS         /16/
      DATA COLS         /78/
C----------------------------------------------------------------------
C
C ****  Create the viewport
C
      ROW    = MIN(ROWS,NBUFFER,PBSAVE)
      COL    = MIN(COLS,MAXCOLS)
      STATUS = SMG$CREATE_VIEWPORT(VDID,1,1,ROW,COL)
      IF ( .NOT. STATUS ) THEN
        CALL MSGSCR(STATUS,'WINDIS')
      ENDIF
C
C ****  Label display
C
      LTITLE  = TRULEN(TITLE(1:LEN(TITLE)))
      STATUS = SMG$LABEL_BORDER(VDID,TITLE(1:LTITLE))
      IF ( .NOT. STATUS ) THEN
        CALL MSGSCR(STATUS,'WINDIS')
      ENDIF
C
C ****  Paste display to pasteboard
C
      STATUS = SMG$PASTE_VIRTUAL_DISPLAY(VDID,
     &                                   PASTID,
     &                                   VIEW_ROW,
     &                                   VIEW_COL)
      IF ( .NOT. STATUS ) THEN
        CALL MSGSCR(STATUS,'WINDIS')
      ENDIF
C
C ****  Set up interrupt
C
      OLD_EVENT_MODE = EVENT_MODE
      IF ( TIMEOUT .GT. 0 ) THEN
        CALL MTIMER('TIMEOUT',TIMEOUT)
      ELSE
        CALL MENUEF(.TRUE.)
      ENDIF
      ASTFLG = .FALSE. 
C
C ****  Manage scrolling
C
      NEWLINE = 1
      NEWCOL  = 1
      ACTIVE  = .TRUE.
      DO WHILE ( ACTIVE )
C
C ****  Wait for key-stroke or timeout
C
        STATUS = LIBKEY(KEYID,VDID,KEY_CODE)
        IF ( .NOT. STATUS ) THEN
          CALL MSGSCR(STATUS,'WINDIS')
        ENDIF
C
C ****  NEXT SCREEN
C
        KEY_CODE = IABS(KEY_CODE)
C
C ****  HOME
C
        IF ( KEY_CODE .EQ. SMG$K_TRM_KP5 ) THEN
          NEWLINE = 1
          NEWCOL  = 1

        ELSEIF ( (KEY_CODE .EQ. SMG$K_TRM_NEXT_SCREEN) .OR.
     &           (KEY_CODE .EQ. SMG$K_TRM_KP2) ) THEN
C
          NEWLINE = MIN(NEWLINE+ROW,NBUFFER-ROW+1)
C
C ****  PREVIOUS SCREEN
C
        ELSEIF ( (KEY_CODE .EQ. SMG$K_TRM_PREV_SCREEN) .OR.
     &           (KEY_CODE .EQ. SMG$K_TRM_KP8) ) THEN
C
          NEWLINE = MAX(NEWLINE-ROW,1)
C
C ****  RIGHT SCREEN
C
        ELSEIF (  KEY_CODE .EQ. SMG$K_TRM_KP6  ) THEN
C
          NEWCOL  = MIN(NEWCOL+COL,MAXCOLS-COL+1)
C
C ****  LEFT SCREEN
C
        ELSEIF (  KEY_CODE .EQ. SMG$K_TRM_KP4  ) THEN
C
          NEWCOL  = MAX(NEWCOL-COL,1)
C
C ****  CURSOR KEYS
C
        ELSEIF (  KEY_CODE .EQ. SMG$K_TRM_UP  ) THEN
          NEWLINE = MAX(NEWLINE-1,1)
C
        ELSEIF (  KEY_CODE .EQ. SMG$K_TRM_DOWN  ) THEN
          NEWLINE = MIN(NEWLINE+1,NBUFFER-ROW+1)
C
        ELSEIF (  KEY_CODE .EQ. SMG$K_TRM_RIGHT  ) THEN
          NEWCOL  = MIN(NEWCOL+1,MAXCOLS-COL+1)
C
        ELSEIF (  KEY_CODE .EQ. SMG$K_TRM_LEFT  ) THEN
          NEWCOL  = MAX(NEWCOL-1,1)
C
C ****  GOLD KEY
C
        ELSEIF ( KEY_CODE .EQ. SMG$K_TRM_PF1  ) THEN
C
C ****  Wait for key-stroke or timeout
C
          STATUS = LIBKEY(KEYID,VDID,KEY_CODE)
          IF ( .NOT. STATUS ) THEN
            CALL MSGSCR(STATUS,'WINDIS')
          ENDIF
C
C ****  BOTTOM-OF-PAGE
C
          KEY_CODE = IABS(KEY_CODE)
C
C ****  RIGHT SCREEN
C
          IF (  KEY_CODE .EQ. SMG$K_TRM_KP6  ) THEN
C
            NEWCOL  = MAXCOLS-COL+1
C
C ****  LEFT SCREEN
C
          ELSEIF (  KEY_CODE .EQ. SMG$K_TRM_KP4  ) THEN
C
            NEWCOL  = 1
C
          ELSEIF     ( KEY_CODE .EQ. SMG$K_TRM_KP2 ) THEN
            NEWLINE = MAX(NBUFFER-ROW+1,1)
C
C ****  TOP-OF-PAGE
C
          ELSEIF ( KEY_CODE .EQ. SMG$K_TRM_KP8 ) THEN
            NEWLINE = 1
          ENDIF
        ELSE
          ACTIVE = .FALSE.
        ENDIF
C
C ****  Change viewport
C
        IF ( ACTIVE ) THEN
          STATUS = SMG$CHANGE_VIEWPORT(VDID,NEWLINE,NEWCOL,ROW,COL)
          IF ( .NOT. STATUS ) THEN
            CALL MSGSCR(STATUS,'WINDIS')
          ENDIF
        ENDIF
      ENDDO
C
      IF ( TIMEOUT .GT. 0 ) THEN
        CALL MTIMER(' ',0)
      ELSE
        CALL MENUEF(.FALSE.)
      ENDIF
C
C ****  Unpaste view display
C
      STATUS = SMG$UNPASTE_VIRTUAL_DISPLAY(VDID,PASTID)
      IF ( .NOT. STATUS ) THEN
        CALL MSGSCR(STATUS,'WINDIS')
      ENDIF
C
C ****  Delete viewport
C
      STATUS = SMG$DELETE_VIEWPORT(VDID)
      IF ( .NOT. STATUS ) THEN
        CALL MSGSCR(STATUS,'WINDIS')
      ENDIF
C
C&ELSE
C&
C&ENDIF
      VIEW_ROW = DEF_VIEW_ROW
      VIEW_COL = DEF_VIEW_COL
      ROWS     = DEF_ROWS
      COLS     = DEF_COLS
      TIMEOUT  = 0
      CALL MENUEF(OLD_EVENT_MODE)
      RETURN
C
      ENTRY WINPOS(IX,IY)
      VIEW_ROW = IX
      VIEW_COL = IY
      RETURN
C
      ENTRY WINSIZ(IR,IC)
      ROWS = IR
      COLS = IC
      RETURN
C
      ENTRY WINTIM(WAITIM)
      TIMEOUT = WAITIM
      RETURN
C
      ENTRY WINDISGET(LINE,OUTSTR)
      LINE   = NEWLINE
      OUTSTR = ' '
      RETURN
C
      ENTRY WINFIL(NITEM,ITEM)
      IF ( FIRST ) THEN
        FIRST = .FALSE.
C
C ****  Create display for window
C
        STATUS = SMG$CREATE_VIRTUAL_DISPLAY(MAXROWS,
     &                                      MAXCOLS,
     &                                      VDID,
     &                                      SMG$M_BORDER)
        IF ( STATUS.eq.0 ) THEN
          CALL MSGSCR(STATUS,'WINDIS')
        ENDIF
      ENDIF
C
C ****  Clear display
C
      IF ( NITEM .LE. 0 ) THEN
        NBUFFER = 0
        CALL SMG$ERASE_DISPLAY(VDID)
        IF ( STATUS.eq.0 ) THEN
          CALL MSGSCR(STATUS,'WINDIS')
        ENDIF
        GOTO 999
      ENDIF
C
C ****  Write lines to display
C
      L = LEN(ITEM(1))
      N = MIN(MAXROWS,NITEM)
      I = 0
      DO WHILE ( I .LT. N )
        I = I + 1
        IF ( NBUFFER .LT. MAXROWS ) THEN
          NBUFFER = NBUFFER + 1
          STATUS = SMG$PUT_LINE(VDID,ITEM(I)(1:L))
          IF ( STATUS.eq.0 ) THEN
            CALL MSGSCR(STATUS,'WINDIS')
          ENDIF
        ELSE
          CALL INTMSG(' ** Maximum buffer size of 500 exceeded')
          I = N
        ENDIF
      ENDDO
  999 RETURN
      END
