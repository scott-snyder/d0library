      INTEGER FUNCTION GZDWAL()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank DWAL 
C-
C-   Returned value  : pointer to Zebra bank DWAL 
C-
C-   Created  19-APR-1989   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZDGEH
      INCLUDE 'D0$LINKS:IZDWAL.LINK'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C----------------------------------------------------------------------
      GZDWAL = 0
      LDGEH = GZDGEH()
      IF ( LDGEH .GT. 0 ) GZDWAL = LC(LDGEH - IZDWAL)
C----------------------------------------------------------------------
  999 RETURN
      END
