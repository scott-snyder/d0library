      FUNCTION GZVPDL(LAYER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank VPDL
C-                         (VTX pedestals for wire layer LAYER)
C-
C-   Created  16-MAR-1989   Peter Grudberg
C-   Updated   4-OCT-1992   Peter M. Grudberg  Use GZVPDH 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZVPDL.LINK'
      INTEGER GZVPDL, GZVPDH
      INTEGER LAYER
C----------------------------------------------------------------------
      GZVPDL = 0
      LVPDH = GZVPDH()
      IF ( LVPDH .GT. 0 ) GZVPDL = LC( LVPDH - IZVPDL - LAYER )
  999 RETURN
      END
