      FUNCTION GZCRCA()
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
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCRCA.LINK'
      INTEGER GZCRCA
      INTEGER GZFRES,LFRES
C----------------------------------------------------------------------
      GZCRCA = 0
      LFRES = GZFRES()
      IF (LFRES .GT. 0) GZCRCA = LQ(LFRES - IZCRCA)
  999 RETURN
      END
