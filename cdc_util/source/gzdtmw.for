      INTEGER FUNCTION GZDTMW(ILAYER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank DTMW for 
C-                         specified CDC layer
C-
C-   Returned value  : pointer to Zebra bank DTMW of this layer
C-   Inputs  : ILAYER  [I] : CDC Layer number  [0:3]
C-
C-   Created  19-APR-1989   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZDTMH, ILAYER
      INCLUDE 'D0$LINKS:IZDTMW.LINK'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C----------------------------------------------------------------------
      GZDTMW= 0
      LDTMH = GZDTMH()
      IF ( LDTMH .GT. 0 ) GZDTMW= LC(LDTMH - IZDTMW - ILAYER)
C----------------------------------------------------------------------
  999 RETURN
      END
