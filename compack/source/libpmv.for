      SUBROUTINE LIBPMV(PAGES)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-    Scrolls the contents of the message window by PAGES pages of
C-    SMGCOM.SPLLIN-3 lines each.
C-
C-   Inputs  :
C-    PAGES: the number of pages to scroll.
C-
C-   Created   8-NOV-1990   Scott Snyder
C-   Updated  23-FEB-1991   Jan S. Hoftun  (Renamed from
C-                                         PAGE_MESSAGE_VIEWPORT)
C-   Updated  31-MAR-1991   Scott Snyder
C-    Add call to LIBIND.
C-   Updated  27-SEP-1991   Herbert Greenlee
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER PAGES
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
      INCLUDE 'D0$INC:SMGCOM.INC'
C
      INTEGER LINES, VIEWPORT_ROW, MSG_DISPLAY_LINES, CROW, CCOL
      LOGICAL ISTAT
C
      LOGICAL SMG$CHANGE_VIEWPORT, SMG$GET_DISPLAY_ATTR,
     &        SMG$GET_VIEWPORT_CHAR, SMG$RETURN_CURSOR_POS,
     &        SMG$SET_CURSOR_ABS
C----------------------------------------------------------------------
C
      IF (SMGON .AND. SPLFLG) THEN
        LINES = PAGES*(SPLLIN-3)
        IF (LINES .EQ. 0) LINES = SIGN(1, PAGES)
C
        ISTAT = SMG$GET_VIEWPORT_CHAR(MINID1, VIEWPORT_ROW)
        IF (.NOT. ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
C
C viewport_row is the number of the virdisp row at the top of the
C  viewport. bump it by the requested number of lines.
C
        VIEWPORT_ROW = VIEWPORT_ROW + LINES
C
C get the current position of the cursor in the virdisp
C
        ISTAT = SMG$RETURN_CURSOR_POS(MINID1, CROW, CCOL)
        IF (.NOT. ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
C
C make sure we don't try to scroll off the ends of the virdisp.
C  (the order of these tests is significant!)
C
        IF (VIEWPORT_ROW + (SPLLIN-2) .GT. CROW)
     &    VIEWPORT_ROW = CROW - (SPLLIN-2) + 1
C
        IF (VIEWPORT_ROW .LT. 1)
     &    VIEWPORT_ROW = 1
C
C move the viewport. but careful! change_viewport moves the cursor
C  location. so be sure to restore it.
C
        ISTAT = SMG$CHANGE_VIEWPORT(MINID1, VIEWPORT_ROW)
        IF (.NOT. ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
C
        ISTAT = SMG$SET_CURSOR_ABS(MINID1, CROW, CCOL)
        IF (.NOT. ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
c
        call libind
C
      ENDIF
  999 RETURN
      END
