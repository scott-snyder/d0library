      FUNCTION GZVGNH()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return pointer to VGNH bank
C-
C-   Returned value  : pointer to VGNH; 0 if nonexistent
C-
C-   Created   4-OCT-1992   Peter M. Grudberg
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZVGNH.LINK'
C
      INTEGER GZVGNH, GZSVTX
C----------------------------------------------------------------------
      GZVGNH = 0
      LSVTX = GZSVTX()
      IF ( LSVTX .GT. 0 ) GZVGNH = LC(LSVTX - IZVGNH)
  999 RETURN
      END
