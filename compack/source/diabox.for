      SUBROUTINE DIABOX(TITLE,NPROMPT,PROMPT,OUTSTR,LENSTR,FLAG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Display a dynamically created dialog box.
C-
C-   Inputs  : TITLE    [C*]  Title of DIALOG box
C-             NPROMPT  [I]   Number of Prompt strings
C-             PROMPT(*)[C*]  Prompt strings
C-             OUTSTR(*)[C*]  Initial strings
C-
C-   Outputs : OUTSTR(*)[C*]  Output strings
C-             LENSTR(*)[I]   Length of strings
C-
C-   Controls: FLAG     [C*]  'C' Clear OUTSTR
C-
C-   Created  13-OCT-1991   Harrison B. Prosper
C-   Updated  19-NOV-1991   Harrison B. Prosper
C-    Improve
C-   Updated  12-DEC-1991   Herbert Greenlee
C-   Updated  30-MAR-1992   Harrison B. Prosper
C-    Add logging
C-   Updated   1-SEP-1992   Harrison B. Prosper
C-    Fix init bug
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) TITLE
      INTEGER NPROMPT
      CHARACTER*(*) PROMPT(*)
      CHARACTER*(*) OUTSTR(*)
      INTEGER LENSTR(*)
      CHARACTER*(*) FLAG
C
      INTEGER IX,IY,IR,IC
      INTEGER WAITIM
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
      INCLUDE 'D0$INC:SMGCOM.INC'
      INCLUDE 'D0$INC:KEYCOM.INC'
C----------------------------------------------------------------------
      INTEGER SMG$CREATE_VIRTUAL_DISPLAY
      INTEGER SMG$CHANGE_VIRTUAL_DISPLAY
      INTEGER SMG$PASTE_VIRTUAL_DISPLAY
      INTEGER SMG$UNPASTE_VIRTUAL_DISPLAY
      INTEGER SMG$ERASE_DISPLAY
      INTEGER SMG$LABEL_BORDER
      INTEGER SMG$READ_STRING
      INTEGER SMG$PUT_CHARS
      INTEGER SMG$SET_CURSOR_ABS
C----------------------------------------------------------------------
C&IF VAXVMS
      INCLUDE '($SMGDEF)'
      INCLUDE '($TRMDEF)'
C&ELSE
C&      INCLUDE 'D0$INC:SMGDEF.DEF'
C&      INCLUDE 'D0$INC:TRMDEF.DEF'
C&ENDIF
C----------------------------------------------------------------------
      INTEGER DEF_DIAL_ROW,DEF_DIAL_COL,DIAL_ROW,DIAL_COL,ROWS,COLS
      INTEGER DEF_ROWS, DEF_COLS
C----------------------------------------------------------------------
      CHARACTER*1 FLG
      CHARACTER*132 INISTR,RESSTR,PRTSTR,DOTS
      INTEGER*2 KEY_CODE
      INTEGER TRULEN,LRESSTR,LPRTSTR,LINISTR,LMAXSTR
      INTEGER STATUS,I,L,N,LTITLE,II,JJ,NN,LINE,NPRT
      INTEGER VDID,ROW,COL,IROW,ICOL
      INTEGER TIMEOUT
      LOGICAL FIRST,ACTIVE,OLD_EVENT_MODE
      SAVE VDID,FIRST
C----------------------------------------------------------------------
      DATA FIRST        /.TRUE./
      DATA DIAL_ROW     /16/
      DATA DIAL_COL     /2/
      DATA TIMEOUT      /0/
      DATA ROWS         /1/
      DATA COLS         /78/
      DATA DOTS /
     &'...............................................................'/
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
C
        DEF_ROWS     = 1
        DEF_COLS     = 78
        DEF_DIAL_ROW = PBSAVE - DEF_ROWS - 2
        DEF_DIAL_COL = 2
C
C ****  Create display for window
C
        STATUS = SMG$CREATE_VIRTUAL_DISPLAY(ROWS,COLS,VDID,SMG$M_BORDER)
        IF ( STATUS.eq.0 ) THEN
          CALL MSGSCR(STATUS,'DIABOX')
        ENDIF
      ENDIF
C
      NPRT    = NPROMPT
      LTITLE  = TRULEN(TITLE(1:LEN(TITLE)))
      LMAXSTR = LEN(OUTSTR(1))
C
C ****  Check whether to read from a file
C
      IF ( .NOT. TRMFLG ) THEN
        CALL RDLOG_BEGIN('DIABOX',*100)
        DO LINE =  1, NPRT
          CALL RDLOG(' ',OUTSTR(LINE),LENSTR(LINE),*100)
        ENDDO
        CALL RDLOG_END('DIABOX',*100)
        GOTO 100
      ENDIF
C
      LPRTSTR = 0
      DO LINE =  1, NPRT
        LENSTR(LINE) = 0
        PRTSTR = PROMPT(LINE)
        LPRTSTR= MAX(TRULEN(PRTSTR),LPRTSTR)
      ENDDO
      LPRTSTR = LPRTSTR + 3
C
C ****  Alter size of dialog box
C
      ROW     = NPRT
      COL     = MAX(LTITLE,LMAXSTR+LPRTSTR+1)
      STATUS  = SMG$CHANGE_VIRTUAL_DISPLAY(VDID,ROW,COL)
      IF ( STATUS.eq.0 ) THEN
        CALL MSGSCR(STATUS,'DIABOX')
      ENDIF
C
C ****  Label dialog box
C
      STATUS = SMG$LABEL_BORDER(VDID,TITLE(1:LTITLE))
      IF ( STATUS.eq.0 ) THEN
        CALL MSGSCR(STATUS,'DIABOX')
      ENDIF
C
C ****  Erase dialog box
C
      STATUS = SMG$ERASE_DISPLAY(VDID)
      IF ( STATUS.eq.0 ) THEN
        CALL MSGSCR(STATUS,'DIABOX')
      ENDIF
C
C ****  Paste dialog box to pasteboard
C
      STATUS = SMG$PASTE_VIRTUAL_DISPLAY(VDID,PASTID,DIAL_ROW,DIAL_COL)
      IF ( STATUS.eq.0 ) THEN
        CALL MSGSCR(STATUS,'DIABOX')
      ENDIF
C
C ****  Cancel event mode
C
      OLD_EVENT_MODE = EVENT_MODE
      IF ( OLD_EVENT_MODE ) THEN
        CALL MENUEF(.FALSE.)
      ENDIF
C
C ****  Display prompts
C
      CALL UPCASE(FLAG(1:1),FLG)
C
      DO LINE = 1, NPRT
        PRTSTR = PROMPT(LINE)(1:LPRTSTR-3)//' | '
        STATUS = SMG$SET_CURSOR_ABS(VDID,LINE,1)
        IF ( FLG .EQ. 'C' ) THEN
          INISTR  = ' '
        ELSE
          INISTR  = OUTSTR(LINE)
        ENDIF
        STATUS = SMG$READ_STRING(KEYID,
     &                           RESSTR(1:LMAXSTR),
     &                           PRTSTR(1:LPRTSTR),%VAL(0),
     &                           TRM$M_TM_TRMNOECHO+
     &                           TRM$M_TM_NORECALL,
     &                           0,%VAL(0),
     &                           LRESSTR,
     &                           KEY_CODE,
     &                           VDID,
     &                           INISTR(1:LMAXSTR))
      ENDDO
C
C ****  Get strings
C
      LINE = 1
      ACTIVE = .TRUE.
      DO WHILE ( ACTIVE )
C
        KEY_CODE = 0
        LRESSTR= 0
        RESSTR = ' '
        PRTSTR = PROMPT(LINE)(1:LPRTSTR-3)//' | '
        INISTR = OUTSTR(LINE)
        LINISTR= TRULEN(INISTR)
C
        STATUS = SMG$SET_CURSOR_ABS(VDID,LINE,1)
C
        IF ( (ICHAR(INISTR(1:1)) .EQ. 0) .OR.
     &       (LINISTR .LE. 0)            ) THEN
C
          IF ( TIMEOUT .GT. 0 ) THEN
            STATUS = SMG$READ_STRING(KEYID,
     &                               RESSTR(1:LMAXSTR),
     &                               PRTSTR(1:LPRTSTR),%VAL(0),
     &                               TRM$M_TM_TRMNOECHO+
     &                               TRM$M_TM_NORECALL,
     &                               TIMEOUT,%VAL(0),
     &                               LRESSTR,
     &                               KEY_CODE,
     &                               VDID, '')
          ELSE
            STATUS = SMG$READ_STRING(KEYID,
     &                               RESSTR(1:LMAXSTR),
     &                               PRTSTR(1:LPRTSTR),%VAL(0),
     &                               TRM$M_TM_TRMNOECHO+
     &                               TRM$M_TM_NORECALL,
     &                               %VAL(0),%VAL(0),
     &                               LRESSTR,
     &                               KEY_CODE,
     &                               VDID, '')
          ENDIF
        ELSE
          IF ( TIMEOUT .GT. 0 ) THEN
            STATUS = SMG$READ_STRING(KEYID,
     &                               RESSTR(1:LMAXSTR),
     &                               PRTSTR(1:LPRTSTR),%VAL(0),
     &                               TRM$M_TM_TRMNOECHO+
     &                               TRM$M_TM_NORECALL,
     &                               TIMEOUT,%VAL(0),
     &                               LRESSTR,
     &                               KEY_CODE,
     &                               VDID,
     &                               INISTR(1:LINISTR))
          ELSE
            STATUS = SMG$READ_STRING(KEYID,
     &                               RESSTR(1:LMAXSTR),
     &                               PRTSTR(1:LPRTSTR),%VAL(0),
     &                               TRM$M_TM_TRMNOECHO+
     &                               TRM$M_TM_NORECALL,
     &                               %VAL(0),%VAL(0),
     &                               LRESSTR,
     &                               KEY_CODE,
     &                               VDID,
     &                               INISTR(1:LINISTR))
          ENDIF
        ENDIF
        IF ( STATUS.eq.0 ) THEN
          CALL MSGSCR(STATUS,'DIABOX')
        ENDIF
C
        IF ( LRESSTR .GT. 0 ) THEN
          OUTSTR(LINE) = RESSTR(1:LRESSTR)
          LENSTR(LINE) = LRESSTR
        ENDIF
C
        IF     ( KEY_CODE .EQ. SMG$K_TRM_UP   ) THEN
          LINE = LINE - 1
        ELSEIF ( KEY_CODE .EQ. 2 )  THEN
          LINE = LINE - 1
        ELSEIF ( KEY_CODE .EQ. SMG$K_TRM_DOWN ) THEN
          LINE = LINE + 1
        ELSEIF ( KEY_CODE .EQ. SMG$K_TRM_PF4  ) THEN
          ACTIVE = .FALSE.
        ELSEIF ( KEY_CODE .EQ. SMG$K_TRM_THIRD_UP  ) THEN
          ACTIVE = .FALSE.
        ELSEIF ( NPRT .EQ. 1 ) THEN
          ACTIVE = .FALSE.
        ELSE
          LINE = LINE + 1
        ENDIF
C
C ****  Wrap around
C
        IF ( LINE .GT. NPRT ) THEN
          LINE = 1
        ENDIF
        IF ( LINE .LT. 1 ) THEN
          LINE = NPRT
        ENDIF
      ENDDO
C
C ****  Unpaste dialog box
C
      STATUS = SMG$UNPASTE_VIRTUAL_DISPLAY(VDID,PASTID)
      IF ( STATUS.eq.0 ) THEN
        CALL MSGSCR(STATUS,'DIABOX')
      ENDIF
C
  100 CONTINUE
C
C ****  Check for logging mode
C
      IF ( LOGUP ) THEN
        CALL WTLOG_BEGIN('DIABOX')
        DO LINE =  1, NPRT
          PRTSTR = PROMPT(LINE)
          LPRTSTR= TRULEN(PRTSTR)
          CALL WTLOG(OUTSTR(LINE),LENSTR(LINE),PRTSTR,LPRTSTR)
        ENDDO
        CALL WTLOG_END('DIABOX')
      ENDIF
C
      DIAL_ROW = DEF_DIAL_ROW
      DIAL_COL = DEF_DIAL_COL
      ROWS     = DEF_ROWS
      COLS     = DEF_COLS
      TIMEOUT  = 0
      CALL MENUEF(OLD_EVENT_MODE)
      RETURN
C
      ENTRY DIAPOS(IX,IY)
      DIAL_ROW = IX
      DIAL_COL = IY
      RETURN
C
      ENTRY DIASIZ(IR,IC)
      COLS = IC
      RETURN
C
      ENTRY DIATIM(WAITIM)
      TIMEOUT = WAITIM
C
  999 RETURN
      END
