      INTEGER FUNCTION GZDGNL(ILAYER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank DGNL for 
C-                         specified CDC layer
C-
C-   Returned value  : pointer to Zebra bank DGNL of this layer
C-   Inputs  : ILAYER  [I] : CDC Layer number  [0:3]
C-
C-   Created  19-APR-1989   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZDGNH, ILAYER
      INCLUDE 'D0$LINKS:IZDGNL.LINK'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C----------------------------------------------------------------------
      GZDGNL = 0
      LDGNH = GZDGNH()
      IF ( LDGNH .GT. 0 ) GZDGNL = LC(LDGNH - IZDGNL - ILAYER)
C----------------------------------------------------------------------
  999 RETURN
      END
