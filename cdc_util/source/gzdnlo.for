      INTEGER FUNCTION GZDNLO (ILAYER)      
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to bank DNLO
C-
C-   Inputs  : ILAYER
C-   Outputs : 
C-   Controls: 
C-
C-   Created   1-OCT-1992   Domenico Pizzuto
C-
C----------------------------------------------------------------------

      IMPLICIT NONE

      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZDNLO.LINK'

      INTEGER ILAYER,LDTVA,GZDTVA

      GZDNLO = 0
      LDTVA  = GZDTVA (ILAYER)
      IF (LDTVA.GT.0) GZDNLO = LC (LDTVA-IZDNLO)

  999 RETURN
      END
