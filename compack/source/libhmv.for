      SUBROUTINE LIBHMV
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-    Moves the message window viewport to the top of the virtual display.
C-
C-   Created   8-NOV-1990   Scott Snyder
C-   Updated  23-FEB-1991   Jan S. Hoftun  (Renamed from HOME_MESSAGE_VIEWPORT)
C-   Updated  31-MAR-1991   Scott Snyder
C-    Added call to LIBIND.
C-   Updated  30-SEP-1991   Herbert Greenlee
C-   Modified    13-AUG-1992 sss - compile on ibm
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
      INCLUDE 'D0$INC:SMGCOM.INC'
C
      LOGICAL ISTAT
      INTEGER CCOL, CROW
C
      LOGICAL SMG$CHANGE_VIEWPORT, SMG$RETURN_CURSOR_POS,
     &        SMG$SET_CURSOR_ABS
C----------------------------------------------------------------------
C
      IF (SMGON .AND. SPLFLG) THEN
C
C move the viewport. but careful! change_viewport moves the cursor
C  location. so be sure to save and restore it.
C
        ISTAT = SMG$RETURN_CURSOR_POS(MINID1, CROW, CCOL)
        IF (.NOT. ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
        ISTAT = SMG$CHANGE_VIEWPORT(MINID1, 1,
     &                              %VAL(0),%VAL(0),%VAL(0))
        IF (.NOT. ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
        ISTAT = SMG$SET_CURSOR_ABS(MINID1, CROW, CCOL)
        IF (.NOT. ISTAT) CALL LIB$SIGNAL(%VAL(ISTAT))
C
        CALL LIBIND
      ENDIF
  999 RETURN
      END
