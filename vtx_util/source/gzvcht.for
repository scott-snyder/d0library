      FUNCTION GZVCHT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  27-OCT-1993   Peter Grudberg
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZVCHT.LINK'
C
      INTEGER GZVCHT
C
      INTEGER LVTXH, GZVTXH
C----------------------------------------------------------------------
C
      GZVCHT = 0
      LVTXH = GZVTXH()
      IF ( LVTXH .GT. 0 ) GZVCHT = LQ(LVTXH-IZVCHT)
  999 RETURN
      END
