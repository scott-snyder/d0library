      FUNCTION GZVMAT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank VMAT
C-                         (VTX material constants)
C-
C-   Created  16-MAR-1989   Peter Grudberg
C-   Updated   4-OCT-1992   Peter M. Grudberg  Use GZVGEH 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZVMAT.LINK'
      INTEGER GZVMAT, GZVGEH
C----------------------------------------------------------------------
      GZVMAT = 0
      LVGEH = GZVGEH()
      IF ( LVGEH .GT. 0 ) GZVMAT = LC( LVGEH - IZVMAT )
  999 RETURN
      END
