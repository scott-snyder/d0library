      INTEGER FUNCTION GZDRFT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank DRFT 
C-
C-   Returned value  : pointer to Zebra bank DRFT 
C-
C-   Created  19-APR-1989   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZDGEH
      INCLUDE 'D0$LINKS:IZDRFT.LINK'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C----------------------------------------------------------------------
      GZDRFT = 0
      LDGEH = GZDGEH()
      IF ( LDGEH .GT. 0 ) GZDRFT = LC(LDGEH - IZDRFT)
C----------------------------------------------------------------------
  999 RETURN
      END
