      FUNCTION GZISAM()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-     get pointer to first ISAM
C-   Returned value  : pointer
C-
C-   D. Hedin 15-oct-90
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZISAM
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZISAM.LINK'
      INTEGER LISAE,GZISAE
C----------------------------------------------------------------------
C
      LISAE=GZISAE()
      IF(LISAE.GT.0) GZISAM=LQ(LISAE-IZISAM)
  999 RETURN
      END
