      FUNCTION GZISV1()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-     Find pointer to ISV1
C-
C-   Created  MAY-20-88 Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZISV1
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZISV1.LINK'
      INTEGER LISAE,GZISAE
C----------------------------------------------------------------------
C
      GZISV1=0
      LISAE=GZISAE()
      IF(LISAE.NE.0)  GZISV1=LQ(LISAE-IZISV1)
C
      RETURN
      END
