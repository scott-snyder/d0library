      FUNCTION GZFITR()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank FITR
C-
C-   Returned value  : FITR bank pointer
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created   3-APR-1989   Jeffrey Bantly
C-   Updated  14-FEB-1992   Susan K. Blessing  Remove machine block. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZFITR.LINK'
C
      INTEGER GZFITR
      INTEGER LKFTRH, GZFTRH
C
C----------------------------------------------------------------------
C
      GZFITR = 0
      LKFTRH=GZFTRH()
      IF ( LKFTRH .NE. 0 ) GZFITR=LQ(LKFTRH-IZFITR)
C----------------------------------------------------------------------
  999 RETURN
      END
