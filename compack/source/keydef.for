      SUBROUTINE KEYDEF(NKEY,KEYNUM,KEYNAME)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create a window containing a display of a
C-   facsimile of the Key-pad. The key-pad can be displayed with a call
C-   to KEYPAD(1) and the display removed by calling KEYPAD(0).
C-
C-   Inputs  : NKEY             [I]     Number of keys (max 18)
C-             KEYNUM(*)        [I]     List of key codes
C-
C-             KEY name         KEY code
C-
C-             KP0 .. KP9       0  -- 9
C-             PF1 .. PF4       10 -- 13
C-             MINUS            14
C-             COMMA            15
C-             ENTER            16
C-             PERIOD           17
C-
C-             KEYNAME(*)       [C*]    List of key names
C-   Outputs : None
C-
C-   Created   7-JUL-1991   Harrison B. Prosper
C-   Updated   6-MAY-1992   Harrison B. Prosper  
C-      Remove check on TRMFLG 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NKEY, KEYNUM(*)
      CHARACTER*(*) KEYNAME(*)
      INTEGER SWIT
      INTEGER KEY_CODE
      CHARACTER*(*) KEY_NAME
C----------------------------------------------------------------------
      LOGICAL GETDEV,FIRST,PASTED
      INTEGER I,J,K,L,N,STATUS,PFWAIT
      INTEGER LINES, WIDTH, ROWS, COLUMNS,START_ROW,START_COL,NTEXT
      INTEGER KEYPAD_ID
C
      INTEGER MAXTOK
      PARAMETER( MAXTOK = 10 )
      INTEGER MAXKEY
      PARAMETER( MAXKEY = 18 )
      INTEGER ROW(MAXKEY),COL(MAXKEY),LIN(MAXKEY),WID(MAXKEY)
      INTEGER LTOKEN(MAXTOK),NTOKEN,IMAP(MAXKEY)
      LOGICAL BOLD(MAXKEY)
      CHARACTER*32 TOKEN(MAXTOK),DEFKEY(MAXKEY),STRING
C
      PARAMETER( NTEXT =  2)
      PARAMETER( LINES =  4)
      PARAMETER( WIDTH = 10)
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:SMGCOM.INC'
C&IF VAXVMS
      INTEGER SMG$CREATE_VIRTUAL_DISPLAY
      INTEGER SMG$PASTE_VIRTUAL_DISPLAY
      INTEGER SMG$UNPASTE_VIRTUAL_DISPLAY
      INTEGER SMG$DRAW_RECTANGLE
      INTEGER SMG$PUT_CHARS
      INTEGER SMG$ERASE_DISPLAY
C----------------------------------------------------------------------
      INCLUDE '($TRMDEF)'
      INCLUDE '($SMGDEF)'
C&ELSE
C&      INCLUDE 'D0$INC:SMGDEF.DEF'
C&      INCLUDE 'D0$INC:TRMDEF.DEF'
C&ENDIF
C----------------------------------------------------------------------
      SAVE FIRST,KEYPAD_ID,START_ROW,START_COL,DEFKEY,PASTED
C----------------------------------------------------------------------
      DATA DEFKEY/'PF1','PF2','PF3','PF4',
     &            'KP7','KP8','KP9','MINUS',
     &            'KP4','KP5','KP6','COMMA',
     &            'KP1','KP2','KP3','ENTER',
     &            'KP0','PERIOD'/
      DATA IMAP/17,
     &          13,14,15,
     &          9,10,11,
     &          5, 6, 7,
     &          1, 2, 3, 4, 8, 12, 16, 18/
      DATA BOLD/18*.FALSE./
C----------------------------------------------------------------------
      DATA KEYPAD_ID  /0/
      DATA PASTED     /.FALSE./
      DATA FIRST      /.TRUE./
C----------------------------------------------------------------------
      IF ( GETDEV() ) THEN
C&IF VAXVMS
C
C ****  Create help window
C
        IF ( FIRST ) THEN
          FIRST = .FALSE.
C
C ****  Define keypad
C
          DO J =  1, MAXKEY
            I = (J+3)/4
            K = MOD(J+3,4)
            COL(J) = 1 + WIDTH*K
            ROW(J) = 1 + LINES*(I-1)
            LIN(J) = LINES
            WID(J) = WIDTH
          ENDDO
          LIN(16) = 2*LINES
          WID(17) = 2*WIDTH
          COL(18) = COL(15)
C
          ROWS = 5*LINES
          COLUMNS = 4*WIDTH
          START_ROW = 3
          START_COL = PBCOLS - 3 - COLUMNS
C
C ****  Create display
C
          STATUS = SMG$CREATE_VIRTUAL_DISPLAY(ROWS,COLUMNS,KEYPAD_ID)
          IF ( .NOT. STATUS ) CALL LIB$SIGNAL (%VAL(STATUS))
          STATUS = SMG$ERASE_DISPLAY (KEYPAD_ID,,,,)
          IF ( .NOT. STATUS ) CALL LIB$SIGNAL (%VAL(STATUS))
C
C ****  Overwrite default names
C
          IF ( NKEY .GT. 0 ) THEN
            DO I = 1, NKEY
              K = KEYNUM(I)
              IF ( (K .GE. 0) .AND. (K .LE. (MAXKEY-1)) ) THEN
                J = IMAP(K+1)
                DEFKEY(J) = KEYNAME(I)
                BOLD(J)   = .TRUE.
              ENDIF
            ENDDO
          ENDIF
C
          DO I = 1,MAXKEY
C
C ****  Write to Box
C
            STRING = DEFKEY(I)
            DO J = 1, NTEXT
              TOKEN(J) = ' '
            ENDDO
            CALL CHOP(STRING,TOKEN,LTOKEN,NTOKEN)
            N = MIN(NTEXT,NTOKEN)
C
            DO J = 1, N
              IF ( BOLD(I) ) THEN
                STATUS = SMG$PUT_CHARS(KEYPAD_ID,
     &                                 TOKEN(J)(1:WID(I)-2),
     &                                 ROW(I)+J,COL(I)+1,,
     &                                 SMG$M_BOLD)
              ELSE
                STATUS = SMG$PUT_CHARS(KEYPAD_ID,
     &                                 TOKEN(J)(1:WID(I)-2),
     &                                 ROW(I)+J,COL(I)+1)
              ENDIF
              IF ( .NOT. STATUS ) CALL LIB$SIGNAL (%VAL(STATUS))
            ENDDO
C
C ****  Draw Box around text
C
            STATUS = SMG$DRAW_RECTANGLE(KEYPAD_ID,
     &                                  ROW(I),COL(I),
     &                                  ROW(I)+LIN(I)-1,COL(I)+WID(I)-1)
            IF ( .NOT. STATUS ) CALL LIB$SIGNAL (%VAL(STATUS))
          ENDDO
        ENDIF
C&ELSE
C&ENDIF
      ENDIF
      RETURN
C
C ****  Paste window to pasteboard
C
      ENTRY KEYPAD(SWIT)
      IF ( GETDEV() ) THEN
C&IF VAXVMS
        IF ( KEYPAD_ID .LE. 0 ) GOTO 999
C
        IF ( SWIT .EQ. 1 ) THEN
          IF ( .NOT. PASTED ) THEN
            PASTED = .TRUE.
            STATUS = SMG$PASTE_VIRTUAL_DISPLAY(KEYPAD_ID,
     &                                         PASTID,
     &                                         START_ROW,
     &                                         START_COL)
            IF ( .NOT. STATUS ) CALL LIB$SIGNAL (%VAL(STATUS))
          ENDIF
        ELSE
C
C ****  Remove key-pad help window from pasteboard
C
          IF ( PASTED ) THEN
            PASTED = .FALSE.
            STATUS = SMG$UNPASTE_VIRTUAL_DISPLAY (KEYPAD_ID,PASTID)
            IF ( .NOT. STATUS ) CALL LIB$SIGNAL (%VAL(STATUS))
          ENDIF
        ENDIF
C&ELSE
C&ENDIF
      ENDIF
      RETURN
C
C ****  Translate key-code
C
      ENTRY KEYNAM(KEY_CODE,KEY_NAME)
      IF     ( (KEY_CODE .GE. SMG$K_TRM_PF1) .AND.
     &         (KEY_CODE .LE. SMG$K_TRM_PF4) ) THEN
        I = KEY_CODE - SMG$K_TRM_PF1 + 10
        KEY_NAME = DEFKEY(IMAP(1+I))
      ELSEIF ( (KEY_CODE .GE. SMG$K_TRM_KP0) .AND.
     &         (KEY_CODE .LE. SMG$K_TRM_KP9) ) THEN
        I = KEY_CODE - SMG$K_TRM_KP0
        KEY_NAME = DEFKEY(IMAP(1+I))
      ELSEIF (  KEY_CODE .EQ. SMG$K_TRM_MINUS ) THEN
        I = 14
        KEY_NAME = DEFKEY(IMAP(1+I))
      ELSEIF (  KEY_CODE .EQ. SMG$K_TRM_COMMA ) THEN
        I = 15
        KEY_NAME = DEFKEY(IMAP(1+I))
      ELSEIF (  KEY_CODE .EQ. SMG$K_TRM_ENTER ) THEN
        I = 16
        KEY_NAME = DEFKEY(IMAP(1+I))
      ELSEIF (  KEY_CODE .EQ. SMG$K_TRM_PERIOD) THEN
        I = 17
        KEY_NAME = DEFKEY(IMAP(1+I))
      ENDIF
  999 RETURN
      END
