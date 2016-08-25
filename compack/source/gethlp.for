      SUBROUTINE GETHLP (PRTID,PRT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Display HELP for given prompt (PRTID,PRT)
C-
C-   Inputs  : PRTID       Prompt identifier
C-             PRT         Prompt
C-
C-   Outputs : 
C-
C-   Created  22-APR-1988   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER       PRTID
      CHARACTER*(*) PRT
      LOGICAL GETDEV,FIRST
      INTEGER STATUS,NLINES,I,J,PFWAIT
      INTEGER LINES,COLUMNS,LINE,COLUMN,HELPID
      PARAMETER( LINES   =  9 )
      PARAMETER( COLUMNS = 72 )
      PARAMETER( LINE    =  3 )
      PARAMETER( COLUMN  = 10 )
      CHARACTER*(COLUMNS) TEXT(LINES)
C
      INTEGER SMG$CREATE_VIRTUAL_DISPLAY
      INTEGER SMG$DELETE_VIRTUAL_DISPLAY
      INTEGER SMG$PASTE_VIRTUAL_DISPLAY
      INTEGER SMG$UNPASTE_VIRTUAL_DISPLAY
      INTEGER SMG$PUT_WITH_SCROLL
      INTEGER SMG$ERASE_DISPLAY
C&IF VAXVMS
      INTEGER  SMG$M_BORDER
      EXTERNAL SMG$M_BORDER
C&ELSE
C&      INCLUDE 'D0$INC:SMGDEF.DEF'
C&ENDIF
C
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:SMGCOM.INC'
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF ( TRMFLG .AND. GETDEV() ) THEN
C
C ****  Create help window
C
C&IF VAXVMS
        IF ( FIRST ) THEN
          FIRST = .FALSE.
          STATUS = SMG$CREATE_VIRTUAL_DISPLAY
     &    (LINES,COLUMNS,HELPID,%LOC(SMG$M_BORDER))
          IF ( .NOT. STATUS ) CALL LIB$SIGNAL (%VAL(STATUS))
        ENDIF
        STATUS = SMG$ERASE_DISPLAY (HELPID,,,,)
C&ENDIF
C
C ****  Paste window to pasteboard
C
        STATUS = SMG$PASTE_VIRTUAL_DISPLAY (HELPID,PASTID,LINE,COLUMN)
        IF ( STATUS.eq.0 ) CALL LIB$SIGNAL (%VAL(STATUS))
C
C ****  Write help info to window
C
        CALL HLPUSR (PRTID,PRT,TEXT,NLINES)
        DO 10 I = 1,NLINES
          STATUS = SMG$PUT_WITH_SCROLL (HELPID,TEXT(I))
          IF ( MOD(I+1,LINES-1) .EQ. 1) THEN
            CALL INTMSG ('          Press PF-key to continue')
            J = PFWAIT ()
          ENDIF
   10   CONTINUE
C
C ****  Wait for PF-key
C
        I = PFWAIT ()
C
C ****  Remove help window from pasteboard
C
        STATUS = SMG$UNPASTE_VIRTUAL_DISPLAY (HELPID,PASTID)
        IF ( STATUS.eq.0 ) CALL LIB$SIGNAL (%VAL(STATUS))
      ENDIF
  999 RETURN
      END
