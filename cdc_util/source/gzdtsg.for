      INTEGER FUNCTION GZDTSG(ILYR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank DTSG 
C-
C-   Returned value  : pointer to Zebra bank DTSG 
C-
C-   Created  19-SEP-1989   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZDTRH, LDTRH, ILYR
      INCLUDE 'D0$LINKS:IZDTSG.LINK'
      INCLUDE 'D0$INC:ZEBCOM.INC'
C----------------------------------------------------------------------
      GZDTSG = 0
      LDTRH = GZDTRH()
      IF (LDTRH .GT. 0) GZDTSG = LQ(LDTRH - IZDTSG - ILYR)
C----------------------------------------------------------------------
  999 RETURN
      END
