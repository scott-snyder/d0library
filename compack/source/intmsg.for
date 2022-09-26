      SUBROUTINE INTMSG(STRING)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Output string to top part of split screen if
C-                         possible. VAX-specific.
C-
C-   Inputs  : STRING: Characters to be output
C-   Outputs : None
C-   Controls: None
C-
C-   Documented 22-SEP-1988   Jan S. Hoftun
C-   Updated  21-OCT-1988   Jan S. Hoftun   (Take out part never executed)
C-   Updated   7-NOV-1990 Scott Snyder - scrolling viewport stuff
C-   Updated  22-FEB-1991 Scott Snyder - restore cursor position properly.
C-   Updated  31-MAR-1991 Scott Snyder - add call to LIBIND.
C-   Updated  23-APR-1991 Scott Snyder - remove call to LIBCOP. (see LIBPST)
C-   Updated  13-MAY-1991 Scott Snyder - restrict maximum number of lines
C-   Updated   3-OCT-1991 Herbert Greenlee
C-   Modified    13-AUG-1992 sss - compile on ibm
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) STRING
C&IF VAXVMS
      INCLUDE '($LIBDEF)'
C&ENDIF
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
      INCLUDE 'D0$INC:SMGCOM.INC'
      INTEGER I,J
      LOGICAL ISTAT
      INTEGER STARTING_ROW, ENDING_ROW, VIEWPORT_ROW, MSG_DISPLAY_LINES
C
      INTEGER SMG$CHANGE_VIEWPORT, SMG$CHANGE_VIRTUAL_DISPLAY,
     &        SMG$CURSOR_ROW, SMG$GET_DISPLAY_ATTR,
     &        SMG$GET_VIEWPORT_CHAR, SMG$PUT_LINE, SMG$SET_CURSOR_ABS
      INTEGER LIBCUR, LIBGET, TRULEN
      LOGICAL GETDEV
      INTEGER SMG$READ_STRING, SMG$CURSOR_COLUMN, SMG$DELETE_VIEWPORT
      INTEGER SMG$CREATE_VIEWPORT, SMG$PASTE_VIRTUAL_DISPLAY
      CHARACTER*80 COMIN
C
      INTEGER MAX_LINES
      PARAMETER (MAX_LINES = 32767) ! SMG uses 16 bits for row #s, anyway
C----------------------------------------------------------------------
      ISTAT=LIBGET(I,J)
      IF(SMGON.AND.GETDEV().AND.SPLFLG) THEN    ! Should it be written
C                                               ! to upper display?
C
C check that there are at least 2*SPLLIN empty lines in the message
C  virdisp; if not, grow the display. (2* to leave room for the
C  possiblity of having to clear the viewport.)
C
        ISTAT = SMG$GET_DISPLAY_ATTR(MINID1, MSG_DISPLAY_LINES,
     &                               %VAL(0),%VAL(0),%VAL(0),
     &                               %VAL(0),%VAL(0))
        IF (.NOT. ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
        STARTING_ROW = SMG$CURSOR_ROW(MINID1)
        IF (MSG_DISPLAY_LINES - STARTING_ROW + 1 .LT. 2*SPLLIN .AND.
     &      MSG_DISPLAY_LINES .LT. MAX_LINES) THEN
          ISTAT = SMG$CHANGE_VIRTUAL_DISPLAY(MINID1,
     &                                       MIN(MSG_DISPLAY_LINES * 2,
     &                                           MAX_LINES),
     &                                       PBCOLS-2,
     &                                       %VAL(0),%VAL(0),%VAL(0))
C
C if we run out of memory, just (silently) start scrolling stuff off the
C     top.
C
C&IF VAXVMS
          IF (.NOT. ISTAT .AND. ISTAT .NE. LIB$_INSVIRMEM)
     &      CALL LIB$SIGNAL(%VAL(ISTAT))
C&ENDIF
C
          ISTAT = SMG$SET_CURSOR_ABS(MINID1, STARTING_ROW,%VAL(0))
          IF (.NOT. ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
        ENDIF
C
        IF(STRING(1:1).EQ.'0') THEN
          ISTAT=SMG$PUT_LINE(MINID1,' ',1,0,%VAL(0),1,%VAL(0),%VAL(0))
        ELSEIF(STRING(1:1).EQ.'1') THEN
          ISTAT = SMG$PUT_LINE(MINID1, ' ', SPLLIN-3,0,%val(0),0,
     &                         %val(0),%val(0))      ! SPLLIN-2 blank lines
          IF (.NOT. ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
        ENDIF
        IF(STRING(1:1).EQ.'+') THEN
          ISTAT=SMG$PUT_LINE(MINID1,STRING(2:TRULEN(STRING)+1),0,0,0,1,
     &      %VAL(0),%VAL(0))
        ELSE
          ISTAT=SMG$PUT_LINE(MINID1,STRING(2:TRULEN(STRING)+1),1,0,0,1,
     &      %VAL(0),%VAL(0))
        ENDIF
C
C If the display or viewport characteristics get modified, scrolling
C  won't happen properly.  Work around this braindamage by forcing
C  scrolling to happen here.
C
        ISTAT = SMG$PUT_LINE(MINID1, ' ', 0,%VAL(0),%VAL(0),%VAL(0),
     &                       %VAL(0),%VAL(0))
C
C if the text cursor was inside the message viewport at the start of
C  this routine (and there is at least one pageful of text in the
C  virdisp), scroll the viewport to show the added text.
C
        ENDING_ROW = SMG$CURSOR_ROW(MINID1)
        ISTAT = SMG$GET_VIEWPORT_CHAR(MINID1, VIEWPORT_ROW,
     &                                %VAL(0),%VAL(0),%VAL(0))
        IF (.NOT. ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
        IF (STARTING_ROW .GE. VIEWPORT_ROW .AND.
     &      STARTING_ROW .LT. VIEWPORT_ROW + SPLLIN-2 .AND.
     &      ENDING_ROW .GT. (SPLLIN-2)) THEN
          ISTAT = SMG$CHANGE_VIEWPORT(MINID1,
     &                                ENDING_ROW - (SPLLIN-2) + 1,
     &                                %VAL(0),%VAL(0),%VAL(0))
          IF (.NOT. ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
          ISTAT = SMG$SET_CURSOR_ABS(MINID1, ENDING_ROW,%VAL(0))
          IF (.NOT. ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
C
          CALL LIBIND
        ENDIF
        IF(.NOT.FULSCR) THEN
          ISTAT=LIBCUR(I,J)  !Reset cursor pos
        ENDIF
      ELSE
        CALL OUTMSG(STRING)
      ENDIF
      RETURN
      END
