      INTEGER FUNCTION GZDPDH()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank DPDH 
C-
C-   Returned value  : pointer to Zebra bank DPDH 
C-
C-   Created  19-APR-1989   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZSCDC
      INCLUDE 'D0$LINKS:IZDPDH.LINK'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C----------------------------------------------------------------------
      GZDPDH = 0
      LSCDC = GZSCDC()
      IF ( LSCDC .GT. 0 ) GZDPDH = LC(LSCDC - IZDPDH)
C----------------------------------------------------------------------
  999 RETURN
      END
