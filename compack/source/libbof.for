      SUBROUTINE LIBBOF
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-    Turn off display batching. All display requests buffered since the
C-    last call to LIBBATCH_ON are performed.
C-
C-   Created   6-FEB-1991   Scott Snyder
C-   Updated  23-FEB-1991   Jan S. Hoftun  (Renamed from LIBBATCH_OFF)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:SMGCOM.INC'
C
      LOGICAL SMG$END_PASTEBOARD_UPDATE
      LOGICAL ISTAT
C----------------------------------------------------------------------
C
      ISTAT = SMG$END_PASTEBOARD_UPDATE(PASTID)
      IF (.NOT.ISTAT) CALL ABOMEN(ISTAT, ' libbatch_off')
  999 RETURN
      END
