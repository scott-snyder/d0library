      FUNCTION GZL2VPDL(LAYER)
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
      INTEGER GZL2VPDL, GZL2VPDH
      INTEGER LAYER,L2VPDH
C----------------------------------------------------------------------
      GZL2VPDL = 0
      L2VPDH = GZL2VPDH()
      IF ( L2VPDH .GT. 0 ) GZL2VPDL = LC( L2VPDH - IZVPDL - LAYER )
  999 RETURN
      END
