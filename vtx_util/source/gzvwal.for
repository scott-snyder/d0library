      FUNCTION GZVWAL()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank VWAL
C-                         (VTX dimensions)
C-
C-   Created  16-MAR-1989   Peter Grudberg
C-   Updated   4-OCT-1992   Peter M. Grudberg  Use GZVGEH 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZVWAL.LINK'
      INTEGER GZVWAL, GZVGEH
C----------------------------------------------------------------------
      GZVWAL = 0
      LVGEH = GZVGEH()
      IF ( LVGEH .GT. 0 ) GZVWAL = LC( LVGEH - IZVWAL )
  999 RETURN
      END
