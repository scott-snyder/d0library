      FUNCTION GZISV2()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-     Find pointer to ISV2
C-
C-   Created  MAY-20-88 Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZISV2
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZISV2.LINK'
      INTEGER LISAE,GZISAE
C----------------------------------------------------------------------
C
      GZISV2=0
      LISAE=GZISAE()
      IF(LISAE.NE.0)  GZISV2=LQ(LISAE-IZISV2)
C
      RETURN
      END
