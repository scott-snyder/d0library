      INTEGER FUNCTION GZDTVA (ILAYER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to bank DTVA
C-
C-   Inputs  : ILAYER
C-   Outputs : 
C-   Controls: 
C-
C-   Created   9-APR-1992   Domenico Pizzuto
C-
C----------------------------------------------------------------------

      IMPLICIT NONE

      INCLUDE 'D0$INC:ZEBSTP.INC'

      INTEGER ILAYER,GZDTVH,LDTVH

      GZDTVA = 0
      LDTVH  = GZDTVH ()
      IF (LDTVH.GT.0) GZDTVA = LC (LDTVH-(ILAYER+1))

  999 RETURN
      END
