      INTEGER FUNCTION GZDALH()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank DALH 
C-
C-   Returned value  : pointer to Zebra bank DALH 
C-
C-   Created  19-APR-1989   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZSCDC
      INCLUDE 'D0$LINKS:IZDALH.LINK'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C----------------------------------------------------------------------
      GZDALH = 0
      LSCDC = GZSCDC()
      IF ( LSCDC .GT. 0 ) GZDALH = LC(LSCDC - IZDALH)
C----------------------------------------------------------------------
  999 RETURN
      END
