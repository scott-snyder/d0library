      FUNCTION GZVPDH()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returen pointer to VPDH bank
C-
C-   Returned value  : pointer to VPDH; 0 if nonexistent
C-
C-   Created   4-OCT-1992   Peter M. Grudberg
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZVPDH.LINK'
C
      INTEGER GZVPDH, GZSVTX
C----------------------------------------------------------------------
      GZVPDH = 0
      LSVTX = GZSVTX()
      IF ( LSVTX .GT. 0 ) GZVPDH = LC(LSVTX - IZVPDH)
  999 RETURN
      END
