      FUNCTION GZVGEH()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return pointer to VGEH bank
C-
C-   Returned value  : pointer to VGEH; 0 if nonexistent
C-
C-   Created   4-OCT-1992   Peter M. Grudberg
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZVGEH.LINK'
C
      INTEGER GZVGEH, GZSVTX
C----------------------------------------------------------------------
      GZVGEH = 0
      LSVTX = GZSVTX()
      IF ( LSVTX .GT. 0 ) GZVGEH = LC(LSVTX - IZVGEH)
  999 RETURN
      END
