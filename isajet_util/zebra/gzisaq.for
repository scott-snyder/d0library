      FUNCTION GZISAQ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-     Find pointer to ISAQ
C-
C-   Created  MAY-20-88 Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZISAQ
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZISAQ.LINK'
      INTEGER LISAE,GZISAE
C----------------------------------------------------------------------
C
      GZISAQ=0
      LISAE=GZISAE()
      IF(LISAE.NE.0)  GZISAQ=LQ(LISAE-IZISAQ)
C
      RETURN
      END
