      SUBROUTINE LIBEMV
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-    Moves the message window viewport to the end of the virtual display.
C-
C-   Created   8-NOV-1990   Scott Snyder
C-   Updated  23-FEB-1991   Jan S. Hoftun  (Renamed from END_MESSAGE_VIEWPORT)
C-   Updated  31-MAR-1991   Scott Snyder
C-    Added call to LIBIND.
C-   Updated  26-SEP-1991   Herbert Greenlee
C-   Modified    13-AUG-1992 sss - compile on ibm
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:MAXLEV.DEF'
      INCLUDE 'D0$INC:COMNUM.INC'
      INCLUDE 'D0$INC:COMCHR.INC'
      INCLUDE 'D0$INC:SMGCOM.INC'
c
      INTEGER viewport_row, ccol, crow
      LOGICAL istat
c
      INTEGER smg$cursor_row
      LOGICAL smg$change_viewport, smg$return_cursor_pos, 
     &  smg$set_cursor_abs
C----------------------------------------------------------------------
c
      IF (smgon .AND. splflg) THEN
        viewport_row = smg$cursor_row(minid1) - (spllin-2) + 1
        IF (viewport_row .LT. 1) viewport_row = 1
c
c move the viewport. but careful! change_viewport moves the cursor
c  location. so be sure to save and restore it.
c
        istat = smg$return_cursor_pos(minid1, crow, ccol)
        IF (.NOT. istat) CALL lib$signal(%val(istat))
        istat = smg$change_viewport(minid1, viewport_row,
     &                              %VAL(0),%VAL(0),%VAL(0))
        IF (.NOT. istat) CALL lib$signal(%val(istat))
        istat = smg$set_cursor_abs(minid1, crow, ccol)
        IF (.NOT. istat) CALL lib$signal(%val(istat))
c
        call libind
      ENDIF
  999 RETURN
      END
