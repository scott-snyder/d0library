      INTEGER FUNCTION GZDLYR(ILYR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank DLYR 
C-
C-   Returned value  : pointer to Zebra bank DLYR 
C-
C-   Created  19-SEP-1989   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZCDCH, LCDCH, ILYR
      INCLUDE 'D0$LINKS:IZDLYR.LINK'
      INCLUDE 'D0$INC:ZEBCOM.INC'
C----------------------------------------------------------------------
      GZDLYR = 0
      LCDCH = GZCDCH()
      IF (LCDCH .GT. 0) GZDLYR = LQ(LCDCH - IZDLYR - ILYR)
C----------------------------------------------------------------------
  999 RETURN
      END
