      INTEGER FUNCTION GZDCBD (ILAYER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return pointer to bank DCBD
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  16-JUN-1992   Domenico Pizzuto
C-
C----------------------------------------------------------------------

      IMPLICIT NONE

      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZDCBD.LINK'

      INTEGER ILAYER,LDTMD,GZDTMD

      GZDCBD = 0
      LDTMD = GZDTMD (ILAYER)
      IF (LDTMD.GT.0) GZDCBD = LC (LDTMD-IZDCBD)

  999 RETURN
      END
