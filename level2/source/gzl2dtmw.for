      INTEGER FUNCTION GZL2DTMW(ILAYER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank DTMW 
C-                         under the  level2 STP header, SL2H
C-                         for the specified CDC layer
C-
C-   Returned value  : pointer to Zebra bank DTMW of this layer
C-   Inputs  : ILAYER  [I] : CDC Layer number  [0:3]
C-
C-   Created  13-MAY-1993   Daniel Claes
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZL2DTMH, ILAYER, L2DTMH
      INCLUDE 'D0$LINKS:IZDTMW.LINK'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C----------------------------------------------------------------------
      GZL2DTMW= 0
      L2DTMH = GZL2DTMH()
      IF ( L2DTMH .GT. 0 ) GZL2DTMW= LC(L2DTMH - IZDTMW - ILAYER)
C----------------------------------------------------------------------
  999 RETURN
      END
