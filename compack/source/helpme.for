      SUBROUTINE HELPME(HELPLIB,TOPIC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Display a help topic from a help library.
C-   VAX-specific.
C-
C-   Inputs  : HELPLIB    [C*]  Help Library
C-             TOPIC      [C*]  Help topic
C-
C-   Outputs :
C-
C-   Controls:
C-
C-   Created  13-OCT-1991   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) HELPLIB
      CHARACTER*(*) TOPIC
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
      INCLUDE 'D0$INC:SMGCOM.INC'
C----------------------------------------------------------------------
C&IF VAXVMS
      INTEGER SMG$CREATE_VIRTUAL_DISPLAY
      INTEGER SMG$PUT_HELP_TEXT
      INTEGER SMG$PASTE_VIRTUAL_DISPLAY
      INTEGER SMG$UNPASTE_VIRTUAL_DISPLAY
      INTEGER SMG$LABEL_BORDER
C----------------------------------------------------------------------
      INCLUDE '($SMGDEF)'
      INCLUDE '($TRMDEF)'
C----------------------------------------------------------------------
      INTEGER HELPLIB_ROW,HELPLIB_COL,ROWS,COLS
      PARAMETER( HELPLIB_ROW = 4)
      PARAMETER( HELPLIB_COL = 2)
      PARAMETER( ROWS   =  18)
      PARAMETER( COLS   =  78)
C----------------------------------------------------------------------
      INTEGER STATUS,I,L,N
      INTEGER HELPLIBID
      CHARACTER*80 HELP,CHOICE
      LOGICAL FIRST
      SAVE FIRST,HELPLIBID
C----------------------------------------------------------------------
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
C
C ****  Create display for helplib menu
C
        STATUS = SMG$CREATE_VIRTUAL_DISPLAY
     &    (ROWS,COLS,HELPLIBID,SMG$M_BORDER)
        IF ( .NOT. STATUS ) THEN
          CALL MSGSCR(STATUS,'Error in HELPME')
        ENDIF
C
        STATUS = SMG$LABEL_BORDER(HELPLIBID,'HELP, Ctrl-Z to Quit')
        IF ( .NOT. STATUS ) THEN
          CALL MSGSCR(STATUS,'Error in HELPME')
        ENDIF
      ENDIF
C
      HELP    = HELPLIB(1:LEN(HELPLIB))
      CHOICE  = TOPIC(1:LEN(TOPIC))
C
C ****  Display help from a help-library
C
      STATUS = SMG$PASTE_VIRTUAL_DISPLAY(HELPLIBID,
     &                                   PASTID,
     &                                   HELPLIB_ROW,
     &                                   HELPLIB_COL)
      IF ( .NOT. STATUS ) THEN
        CALL MSGSCR(STATUS,'PASTE_VIRTUAL_DISPLAY')
      ENDIF
C
      IF ( HELPLIB(1:1) .EQ. ' ' ) THEN
        STATUS = SMG$PUT_HELP_TEXT(HELPLIBID,
     &                             KEYID,
     &                             CHOICE)
      ELSE
        STATUS = SMG$PUT_HELP_TEXT(HELPLIBID,
     &                             KEYID,
     &                             CHOICE,
     &                             HELP)
      ENDIF
      IF ( .NOT. STATUS ) THEN
        CALL MSGSCR(STATUS,'PUT_HELP_TEXT')
      ENDIF
C
      STATUS = SMG$UNPASTE_VIRTUAL_DISPLAY(HELPLIBID,PASTID)
      IF ( .NOT. STATUS ) THEN
        CALL MSGSCR(STATUS,'UNPASTE_VIRTUAL_DISPLAY')
      ENDIF
C&ENDIF
  999 RETURN
      END
