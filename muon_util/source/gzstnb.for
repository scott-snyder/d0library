      INTEGER FUNCTION GZSTNB()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : returns pointer to SAMUS tracks at NORTH
C-                         before magnet bank    
C-
C-   Returned value  : STNB bank address (or zero if something is bad)
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
      INCLUDE 'D0$LINKS:IZSTNB.LINK'
      INTEGER GZSATN, LSATN
      GZSTNB = 0
      LSATN = GZSATN()
      IF(LSATN .NE. 0) THEN
        GZSTNB = LQ(LSATN-IZSTNB) 
      ENDIF
  999 RETURN
      END
