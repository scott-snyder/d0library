      INTEGER FUNCTION GZVALS(LAYER,SECTOR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank VALS
C-                         (VTX alignment constants for wire
C-                          layer LAYER, sector SECTOR)
C-
C-   Created  16-MAR-1989   Peter Grudberg
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZVALS.LINK'
      INTEGER LVALL, GZVALL, LAYER, SECTOR
      GZVALS = 0
      LVALL = GZVALL(LAYER)
      IF ( LVALL .GT. 0 ) GZVALS = LC( LVALL - IZVALS - SECTOR )
  999 RETURN
      END
