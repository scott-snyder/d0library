C DEC/CMS REPLACEMENT HISTORY, Element GZFRES.FOR
C *1     6-NOV-1990 13:26:12 HOFTUN "Get link to the FRES bank"
C DEC/CMS REPLACEMENT HISTORY, Element GZFRES.FOR
      FUNCTION GZFRES()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Returned value  :
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  28-SEP-1990   Richard V. Astur
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZFRES
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZFRES.LINK'
      INTEGER GZFILT,LFILT
C----------------------------------------------------------------------
      GZFRES = 0
      LFILT = GZFILT()
      IF (LFILT .GT. 0) GZFRES = LQ(LFILT - IZFRES)
  999 RETURN
      END
