      INTEGER FUNCTION GZDTMH()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank DTMH 
C-
C-   Returned value  : pointer to Zebra bank DTMH 
C-
C-   Created  19-APR-1989   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZSCDC
      INCLUDE 'D0$LINKS:IZDTMH.LINK'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C----------------------------------------------------------------------
      GZDTMH = 0
      LSCDC = GZSCDC()
      IF ( LSCDC .GT. 0 ) GZDTMH = LC(LSCDC - IZDTMH)
C----------------------------------------------------------------------
  999 RETURN
      END
