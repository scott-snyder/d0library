      INTEGER FUNCTION GZDGEH()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank DGEH 
C-
C-   Returned value  : pointer to Zebra bank DGEH 
C-
C-   Created  19-APR-1989   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZSCDC
      INCLUDE 'D0$LINKS:IZDGEH.LINK'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C----------------------------------------------------------------------
      GZDGEH = 0
      LSCDC = GZSCDC()
      IF ( LSCDC .GT. 0 ) GZDGEH = LC(LSCDC-IZDGEH)
C----------------------------------------------------------------------
  999 RETURN
      END
