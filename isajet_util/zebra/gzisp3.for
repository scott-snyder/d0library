      FUNCTION GZISP3()
C----------------------------------------------------------------------
C-
C-   Returned value  : pointer to first ISP3 bank
C-
C-   Created  23-MAY-1991   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZISP3
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZISP3.LINK'
      INTEGER LISAE,GZISAE
C----------------------------------------------------------------------
      GZISP3=0
      LISAE=GZISAE()
      IF(LISAE.NE.0) GZISP3=LQ(LISAE-IZISP3)
  999 RETURN
      END
