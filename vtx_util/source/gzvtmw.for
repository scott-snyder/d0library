      FUNCTION GZVTMW(LAYER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank VTMW
C-                         (VTX time constants for wire layer LAYER)
C-
C-   Created  16-MAR-1989   Peter Grudberg
C-   Updated   4-OCT-1992   Peter M. Grudberg  Use GZVTMH 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZVTMW.LINK'
      INTEGER GZVTMW, GZVTMH
      INTEGER LAYER
C----------------------------------------------------------------------
      GZVTMW = 0
      LVTMH = GZVTMH()
      IF ( LVTMH .GT. 0 ) GZVTMW = LC( LVTMH - IZVTMW - LAYER )
  999 RETURN
      END
