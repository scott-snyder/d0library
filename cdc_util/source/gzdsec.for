      INTEGER FUNCTION GZDSEC(ISEC,ILYR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank DSEC 
C-
C-   Returned value  : pointer to Zebra bank DSEC 
C-
C-   Created  19-SEP-1989   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZDLYR, LDLYR, ISEC, ILYR
      INCLUDE 'D0$LINKS:IZDSEC.LINK'
      INCLUDE 'D0$INC:ZEBCOM.INC'
C----------------------------------------------------------------------
      GZDSEC = 0
      LDLYR = GZDLYR(ILYR)
      IF (LDLYR .GT. 0) GZDSEC = LQ(LDLYR - IZDSEC - ISEC)
C----------------------------------------------------------------------
  999 RETURN
      END
