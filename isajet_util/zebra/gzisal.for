      FUNCTION GZISAL()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-     get pointer to first ISAL
C-   Returned value  : pointer
C-
C-   Created  17-AUG-1989   Serban Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZISAL
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZISAL.LINK'
      INTEGER LISAE,GZISAE
C----------------------------------------------------------------------
C
      GZISAL = 0
      LISAE=GZISAE()
      IF(LISAE.GT.0) GZISAL=LQ(LISAE-IZISAL)
  999 RETURN
      END
