      INTEGER FUNCTION GZDALS(ILAYER,ISECT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank DALS for 
C-                         specified CDC layer and sector
C-
C-   Returned value  : pointer to Zebra bank DALS of this layer
C-   Inputs  : ILAYER  [I] : CDC Layer number  [0:3]
C-             ISECT   [I] : CDC Sector number  [0:31]
C-
C-   Created  19-APR-1989   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZDALL, ILAYER, ISECT, LDALL(0:3)
      INCLUDE 'D0$LINKS:IZDALS.LINK'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C----------------------------------------------------------------------
      GZDALS = 0
      LDALL(ILAYER) = GZDALL(ILAYER)
      IF ( LDALL(ILAYER) .GT. 0 ) 
     &  GZDALS = LC(LDALL(ILAYER) - IZDALS - ISECT)
C----------------------------------------------------------------------
  999 RETURN
      END
