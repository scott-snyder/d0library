      INTEGER FUNCTION GZFILT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return pointer to FILT bank
C-
C-   Returned value  : zebra link to FILT
C-
C-   Created  22-NOV-1989   Richard V. Astur
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZFILT.LINK'
C----------------------------------------------------------------------
      GZFILT = LQ(LHEAD - IZFILT)
  999 RETURN
      END
