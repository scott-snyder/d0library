      INTEGER FUNCTION GZSATN()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : returns pointer to SAMUS tracks at North
C-                         bank    
C-
C-   Returned value  : SATN bank address (or zero if something is bad)
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
      INCLUDE 'D0$LINKS:IZSATN.LINK'
      INTEGER GZSAMT, LSAMT
      GZSATN = 0
      LSAMT = GZSAMT()
      IF(LSAMT .NE. 0) THEN
        GZSATN = LQ(LSAMT-IZSATN) 
      ENDIF
  999 RETURN
      END
