      FUNCTION GZVRFT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank VRFT
C-                         (VTX design values for wire layers)
C-
C-   Created  16-MAR-1989   Peter Grudberg
C-   Updated   4-OCT-1992   Peter M. Grudberg  Use GZVGEH 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZVRFT.LINK'
      INTEGER GZVRFT, GZVGEH
C----------------------------------------------------------------------
      GZVRFT = 0
      LVGEH = GZVGEH()
      IF ( LVGEH .GT. 0 ) GZVRFT = LC( LVGEH - IZVRFT )
  999 RETURN
      END
