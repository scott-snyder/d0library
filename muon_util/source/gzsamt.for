      INTEGER FUNCTION GZSAMT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : returns pointer to SAMUS track reconstruction
C-                         header bank    
C-
C-   Returned value  : SAMT bank address (or zero if something is bad)
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
      INCLUDE 'D0$LINKS:IZSAMT.LINK'
      INTEGER GZMTRH, LMTRH
      GZSAMT = 0
      LMTRH = GZMTRH()
      IF(LMTRH .NE. 0) THEN
        GZSAMT = LQ(LMTRH-IZSAMT) 
      ENDIF
  999 RETURN
      END
