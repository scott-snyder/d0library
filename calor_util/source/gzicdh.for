      INTEGER FUNCTION GZICDH()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : GZICDH returns the link to the ICDH bank
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  23-JUN-1989   ZACHARY WOLF
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
C--   ZEBRA
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZICDH.LINK/LIST'
C
C--   INTERNAL VARIABLES
      INTEGER LGHIT,GZGHIT
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZICDH=0
C
C--   GET LINK OF SUPPORTING GHIT BANK
      LGHIT=GZGHIT()
      IF(LGHIT.LE.0)GO TO 999
C
C--   RETURN LINK
      GZICDH=LQ(LGHIT-IZICDH)
C
  999 RETURN
      END
