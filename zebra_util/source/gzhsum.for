      INTEGER FUNCTION GZHSUM()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return pointer to HSUM bank
C-
C-   Returned value  : zebra link to HSUM
C-
C-   Updated  18-JAN-1992   James T. Linnemann   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZHSUM.LINK'
C----------------------------------------------------------------------
      GZHSUM = LQ(LHEAD - IZHSUM)
  999 RETURN
      END
