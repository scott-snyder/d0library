      FUNCTION GZVALH()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank VALH
C-                         (VTX alignment header bank)
C-
C-   Created  16-MAR-1989   Peter Grudberg
C-   Updated   4-OCT-1992   Peter M. Grudberg  Use GZSVTX 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZVALH.LINK'
      INTEGER GZVALH, GZSVTX
C----------------------------------------------------------------------
      GZVALH = 0
      LSVTX = GZSVTX()
      IF ( LSVTX .GT. 0 ) GZVALH = LC( LSVTX - IZVALH )
  999 RETURN
      END
