      INTEGER FUNCTION GZVALL(LAYER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank VALL
C-                         (VTX alignment layer level)
C-
C-   Created  16-MAR-1989   Peter Grudberg
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZVALL.LINK'
      INTEGER LVALH, GZVALH, LAYER
      GZVALL = 0
      LVALH = GZVALH()
      IF ( LVALH .GT. 0 ) GZVALL = LC( LVALH - IZVALL - LAYER )
  999 RETURN
      END
