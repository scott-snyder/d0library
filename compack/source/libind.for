      SUBROUTINE LIBIND
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-    Put an indicator at the bottom of the message window iff there
C-    is text scrolled off the bottom.
C-
C-   Created  31-MAR-1991   Scott Snyder
C-   Revised   2-APR-1991   Scott Snyder
C-    Add entry LIBIOF to force the indicator off.
C-    Add test of SMGON and SPLFLG.
C-   Updated  27-SEP-1991   Herbert Greenlee
C-   Modified    13-AUG-1992 sss - compile on ibm
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
C&IF VAXVMS
      INCLUDE '($SMGDEF)'
C&ELSE
C&      INCLUDE 'D0$INC:SMGDEF.DEF'
C&ENDIF
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
      INCLUDE 'D0$INC:SMGCOM.INC'
C
      INTEGER VIEWPORT_ROW, CURSOR_ROW, MESSAGE_ROW, FLAGS
C
      INTEGER SMG$CURSOR_ROW
      LOGICAL SMG$CREATE_VIRTUAL_DISPLAY, 
     &        SMG$GET_PASTING_INFO, SMG$PASTE_VIRTUAL_DISPLAY,
     &        SMG$PUT_CHARS, SMG$UNPASTE_VIRTUAL_DISPLAY,
     &        SMG$GET_VIEWPORT_CHAR
C
      INTEGER INDICATOR_ID
      LOGICAL INDICATOR_PASTED, ISTAT
      SAVE INDICATOR_ID, INDICATOR_PASTED
      DATA INDICATOR_ID/0/
      DATA INDICATOR_PASTED/.FALSE./
C----------------------------------------------------------------------
C
      IF (SMGON .AND. SPLFLG) THEN
C
C Test to see if we're at the end of the message buffer.  We are if the
C  cursor is either visible in the viewport or on the next line following
C  the viewport.
C
        ISTAT = SMG$GET_VIEWPORT_CHAR(MINID1, VIEWPORT_ROW,
     &                                %VAL(0),%VAL(0),%VAL(0))
        IF (.NOT. ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
        CURSOR_ROW = SMG$CURSOR_ROW(MINID1)
        IF (CURSOR_ROW .GT. VIEWPORT_ROW+SPLLIN-2) THEN
C
C we're not at the end.  paste on the indicator virdisp if needed.
C
          IF (.NOT. INDICATOR_PASTED) THEN
C
C create the indicator virdisp if we haven't yet done so.
C  note that ``' is a diamond in the graphics character set.
C
            IF (INDICATOR_ID .EQ. 0) THEN
C&IF VAXVMS
              ISTAT = SMG$CREATE_VIRTUAL_DISPLAY(1, 2, INDICATOR_ID)
              IF (.NOT. ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
              ISTAT = SMG$PUT_CHARS(INDICATOR_ID, '``', , , ,
     &                              SMG$M_BOLD, , SMG$C_SPEC_GRAPHICS)
C&ELSE
C&              ISTAT = SMG$CREATE_VIRTUAL_DISPLAY(1, 6, INDICATOR_ID)
C&              IF (.NOT. ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
C&              ISTAT = SMG$PUT_CHARS(INDICATOR_ID, '<more>',
C&     &                              %VAL(0),%VAL(0),%VAL(0),
C&     &                              %VAL(0),%VAL(0),%VAL(0))
C&ENDIF
              IF (.NOT. ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
            ENDIF
C
C paste the indicator virdisp in the middle of the bottom line of the
C  message window border.
C
            ISTAT = SMG$GET_PASTING_INFO(MINID1, PASTID, FLAGS,
     &                                   MESSAGE_ROW)
            IF (.NOT. ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
C&IF VAXVMS
            ISTAT = SMG$PASTE_VIRTUAL_DISPLAY(INDICATOR_ID, PASTID,
     &                                        MESSAGE_ROW + SPLLIN-2,
     &                                        (PBCOLS-2)/2 + 1)
C&ELSE
C&            ISTAT = SMG$PASTE_VIRTUAL_DISPLAY(INDICATOR_ID, PASTID,
C&     &                                        MESSAGE_ROW + SPLLIN-2,
C&     &                                        (PBCOLS-6)/2 + 1)
C&ENDIF
            IF (.NOT. ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
            INDICATOR_PASTED = .TRUE.
          ENDIF
        ELSE
C
C we're at the end.  make sure the indicator virdisp is not pasted.
C
          IF (INDICATOR_PASTED) THEN
            ISTAT = SMG$UNPASTE_VIRTUAL_DISPLAY(INDICATOR_ID, PASTID)
            IF (.NOT. ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
            INDICATOR_PASTED = .FALSE.
          ENDIF
        ENDIF
      ENDIF
      RETURN
C
C
C ENTRY libiof - force the indicator off
C
      ENTRY LIBIOF
      IF (INDICATOR_PASTED) THEN
        ISTAT = SMG$UNPASTE_VIRTUAL_DISPLAY(INDICATOR_ID, PASTID)
        IF (.NOT. ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
        INDICATOR_PASTED = .FALSE.
      ENDIF
  999 RETURN
      END
