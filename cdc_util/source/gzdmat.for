      INTEGER FUNCTION GZDMAT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank DMAT 
C-
C-   Returned value  : pointer to Zebra bank DMAT 
C-
C-   Created  19-APR-1989   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZDGEH
      INCLUDE 'D0$LINKS:IZDMAT.LINK'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C----------------------------------------------------------------------
      GZDMAT = 0
      LDGEH = GZDGEH()
      IF ( LDGEH .GT. 0 ) GZDMAT = LC(LDGEH - IZDMAT)
C----------------------------------------------------------------------
  999 RETURN
      END
