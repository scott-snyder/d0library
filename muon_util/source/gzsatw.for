      INTEGER FUNCTION GZSATW()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : returns pointer to SAMUS track reconstruction
C-                         work bank
C-
C-   Returned value  : SATW bank address (or zero if something is bad)
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
      INCLUDE 'D0$LINKS:IZSATW.LINK'
      INTEGER GZSAMT, LSAMT
      GZSATW = 0
      LSAMT = GZSAMT()
      IF(LSAMT .NE. 0) THEN
        GZSATW = LQ(LSAMT-IZSATW) 
      ENDIF
  999 RETURN
      END
