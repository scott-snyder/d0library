      INTEGER FUNCTION GZVZST
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank VZST
C-                         (VTX design values for z-strips)
C-
C-   Created  16-MAR-1989   Peter Grudberg
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZVZST.LINK'
      GZVZST = 0
      IF ( LVGEH .GT. 0 ) GZVZST = LC( LVGEH - IZVZST )
  999 RETURN
      END
