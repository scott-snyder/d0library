      FUNCTION GZVTMH()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return pointer to VTMH bank
C-
C-   Returned value  : pointer to VTMH; 0 if nonexistent
C-
C-   Created   4-OCT-1992   Peter M. Grudberg
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZVTMH.LINK'
C
      INTEGER GZVTMH, GZSVTX
C----------------------------------------------------------------------
      GZVTMH = 0
      LSVTX = GZSVTX()
      IF ( LSVTX .GT. 0 ) GZVTMH = LC(LSVTX - IZVTMH)
  999 RETURN
      END
