      FUNCTION GZISAJ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-     Find pointer to ISAJ
C-
C-   Created  MAY-20-88 Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZISAJ
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZISAJ.LINK'
      INTEGER LISAE,GZISAE
C----------------------------------------------------------------------
C
      GZISAJ=0
      LISAE=GZISAE()
      IF(LISAE.NE.0)  GZISAJ=LQ(LISAE-IZISAJ)
C
      RETURN
      END
