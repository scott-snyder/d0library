      INTEGER FUNCTION GZSTNA()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : returns pointer to SAMUS tracks at NORTH
C-                         after magnet bank    
C-
C-   Returned value  : STNA bank address (or zero if something is bad)
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
      INCLUDE 'D0$LINKS:IZSTNA.LINK'
      INTEGER GZSATN, LSATN
      GZSTNA = 0
      LSATN = GZSATN()
      IF(LSATN .NE. 0) THEN
        GZSTNA = LQ(LSATN-IZSTNA) 
      ENDIF
  999 RETURN
      END
