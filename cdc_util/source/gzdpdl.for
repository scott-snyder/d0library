      INTEGER FUNCTION GZDPDL(ILAYER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank DPDL for 
C-                         specified CDC layer
C-
C-   Returned value  : pointer to Zebra bank DPDL of this layer
C-   Inputs  : ILAYER  [I] : CDC Layer number  [0:3]
C-
C-   Created  19-APR-1989   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZDPDH, ILAYER
      INCLUDE 'D0$LINKS:IZDPDL.LINK'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C----------------------------------------------------------------------
      GZDPDL = 0
      LDPDH = GZDPDH()
      IF ( LDPDH .GT. 0 ) GZDPDL = LC(LDPDH - IZDPDL - ILAYER)
C----------------------------------------------------------------------
  999 RETURN
      END
