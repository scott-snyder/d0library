      INTEGER FUNCTION GZSTSA()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : returns pointer to SAMUS tracks at SOUTH
C-                         after magnet bank    
C-
C-   Returned value  : STSA bank address (or zero if something is bad)
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  23-NOV-1993   Vladimir Podstavkov
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZSTSA.LINK'
      INTEGER GZSATS, LSATS
      GZSTSA = 0
      LSATS = GZSATS()
      IF(LSATS .NE. 0) THEN
        GZSTSA = LQ(LSATS-IZSTSA) 
      ENDIF
  999 RETURN
      END
