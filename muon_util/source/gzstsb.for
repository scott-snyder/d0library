      INTEGER FUNCTION GZSTSB()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : returns pointer to SAMUS tracks at SOUTH
C-                         before magnet bank    
C-
C-   Returned value  : STSB bank address (or zero if something is bad)
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
      INCLUDE 'D0$LINKS:IZSTSB.LINK'
      INTEGER GZSATS, LSATS
      GZSTSB = 0
      LSATS = GZSATS()
      IF(LSATS .NE. 0) THEN
        GZSTSB = LQ(LSATS-IZSTSB) 
      ENDIF
  999 RETURN
      END
