      FUNCTION GZVHIT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return pointer to ZEBRA bank VHIT
C-
C-   Returned value  : pointer to VHIT
C-
C-   Created  23-AUG-1991   Peter M. Grudberg
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZVHIT.LINK'
      INTEGER GZVHIT, GZVTXH, LVTXH
C----------------------------------------------------------------------
      GZVHIT = 0
      LVTXH = GZVTXH()
      IF ( LVTXH .GT. 0 ) GZVHIT = LQ(LVTXH - IZVHIT)
  999 RETURN
      END
