      SUBROUTINE LIBBON
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-    Turn on display batching, if possible. Display requests are
C-    buffered until a corresponding call to LIBBATCH_OFF.
C-
C-   Created   6-FEB-1991   Scott Snyder
C-   Updated  23-FEB-1991   Jan S. Hoftun  (Renamed from LIBBATCH_ON) 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:SMGCOM.INC'
C
      LOGICAL SMG$BEGIN_PASTEBOARD_UPDATE
      LOGICAL ISTAT
C----------------------------------------------------------------------
C
      ISTAT = SMG$BEGIN_PASTEBOARD_UPDATE(PASTID)
      IF (.NOT.ISTAT) CALL ABOMEN(ISTAT, ' libbatch_on')
  999 RETURN
      END
