      FUNCTION GZISAC()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      Get pointer to ISAC
C-
C-   Created   8-DEC-1988   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZISAC.LINK'
      INTEGER LISAE,GZISAC,GZISAE
C----------------------------------------------------------------------
C
      GZISAC=0
      LISAE=GZISAE()
      IF(LISAE.NE.0) GZISAC=LQ(LISAE-IZISAC)
  999 RETURN
      END
