      INTEGER FUNCTION GZL0VT()
C----------------------------------------------------------------------
C-
C-   PURPOSE AND METHODS :RETURNS THE LINK ADDRESS TO L0VT BANK 
C-
C-   INPUTS  : NONE 
C-   OUTPUTS : NONE 
C-   CONTROLS: NONE
C-
C-   CREATED   4-JUN-1992   TOM FAHLAND
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZL0VT.LINK'    !CHANGE TO D0$LINK WHEN OFFICIAL
      INTEGER LFRES,GZFRES
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZL0VT = 0
C
C--   GET LINK TO SUPPORTING FRES BANK
      LFRES = GZFRES()
C
      IF(LFRES.NE.0) GZL0VT = LQ(LFRES - IZL0VT)
C----------------------------------------------------------------------
  999 RETURN
      END
