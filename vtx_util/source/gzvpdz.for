      INTEGER FUNCTION GZVPDZ(ZLAYER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank VPDZ
C-                         (VTX pedestals for strip layer ZLAYER)
C-
C-   Created  16-MAR-1989   Peter Grudberg
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZVPDZ.LINK'
      INTEGER ZLAYER
      GZVPDZ = 0
      IF ( LVPDH .GT. 0 ) GZVPDZ = LC( LVPDH - IZVPDZ - ZLAYER )
  999 RETURN
      END
