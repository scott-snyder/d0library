      FUNCTION GZVGNL(LAYER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank VGNL
C-                         (VTX gains for wire layer LAYER)
C-
C-   Created  16-MAR-1989   Peter Grudberg
C-   Updated   4-OCT-1992   Peter M. Grudberg  Use GZVGNH 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZVGNL.LINK'
      INTEGER GZVGNL, GZVGNH
      INTEGER LAYER
C----------------------------------------------------------------------
      GZVGNL = 0
      LVGNH = GZVGNH()
      IF ( LVGNH .GT. 0 ) GZVGNL = LC( LVGNH - IZVGNL - LAYER )
  999 RETURN
      END
