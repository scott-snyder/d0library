      INTEGER FUNCTION GZSAHS()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : returns pointer to SAMUS hits summary
C-                         bank    
C-
C-   Returned value  : SAHS bank address (or zero if something is bad)
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
      INCLUDE 'D0$LINKS:IZSAHS.LINK'
      INTEGER GZSAMT, LSAMT
      GZSAHS = 0
      LSAMT = GZSAMT()
      IF(LSAMT .NE. 0) THEN
        GZSAHS = LQ(LSAMT-IZSAHS) 
      ENDIF
  999 RETURN
      END
