      INTEGER FUNCTION GZSATS()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : returns pointer to SAMUS tracks at South
C-                         bank    
C-
C-   Returned value  : SATS bank address (or zero if something is bad)
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
      INCLUDE 'D0$LINKS:IZSATS.LINK'
      INTEGER GZSAMT, LSAMT
      GZSATS = 0
      LSAMT = GZSAMT()
      IF(LSAMT .NE. 0) THEN
        GZSATS = LQ(LSAMT-IZSATS) 
      ENDIF
  999 RETURN
      END
