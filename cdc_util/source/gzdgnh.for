      INTEGER FUNCTION GZDGNH()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank DGNH 
C-
C-   Returned value  : pointer to Zebra bank DGNH 
C-
C-   Created  19-APR-1989   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZSCDC
      INCLUDE 'D0$LINKS:IZDGNH.LINK'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C----------------------------------------------------------------------
      GZDGNH = 0
      LSCDC = GZSCDC()
      IF ( LSCDC .GT. 0 ) GZDGNH = LC(LSCDC - IZDGNH)
C----------------------------------------------------------------------
  999 RETURN
      END
