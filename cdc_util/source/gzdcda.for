      INTEGER FUNCTION GZDCDA(ISEC,ILYR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank DCDA 
C-
C-   Returned value  : pointer to Zebra bank DCDA 
C-
C-   Created  19-SEP-1989   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZDSEC, LDSEC, ISEC, ILYR
      INCLUDE 'D0$LINKS:IZDCDA.LINK'
      INCLUDE 'D0$INC:ZEBCOM.INC'
C----------------------------------------------------------------------
      GZDCDA = 0
      LDSEC = GZDSEC(ISEC,ILYR)
      IF (LDSEC .GT. 0) GZDCDA = LQ(LDSEC - IZDCDA)
C----------------------------------------------------------------------
  999 RETURN
      END
