      INTEGER FUNCTION GZDTMD(ILAYER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank DTMD for 
C-                         specified CDC layer
C-
C-   Returned value  : pointer to Zebra bank DTMD of this layer
C-   Inputs  : ILAYER  [I] : CDC Layer number  [0:3]
C-
C-   Created  19-APR-1989   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZDTMH, ILAYER
      INCLUDE 'D0$LINKS:IZDTMD.LINK'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C----------------------------------------------------------------------
      GZDTMD= 0
      LDTMH = GZDTMH()
      IF ( LDTMH .GT. 0 ) GZDTMD= LC(LDTMH - IZDTMD - ILAYER)
C----------------------------------------------------------------------
  999 RETURN
      END
