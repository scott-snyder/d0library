      INTEGER FUNCTION GZDALL(ILAYER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank DALL for 
C-                         specified CDC layer
C-
C-   Returned value  : pointer to Zebra bank DALL of this layer
C-   Inputs  : ILAYER  [I] : CDC Layer number  [0:3]
C-
C-   Created  19-APR-1989   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZDALH, ILAYER
      INCLUDE 'D0$LINKS:IZDALL.LINK'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C----------------------------------------------------------------------
      GZDALL = 0
      LDALH = GZDALH()
      IF ( LDALH .GT. 0 ) GZDALL = LC(LDALH - IZDALL - ILAYER)
C----------------------------------------------------------------------
  999 RETURN
      END
