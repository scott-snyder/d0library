      INTEGER FUNCTION GZDNLI (ILAYER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to bank DNLI
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
      INCLUDE 'D0$LINKS:IZDNLI.LINK'

      INTEGER ILAYER,GZDTVA,LDTVA

      GZDNLI = 0
      LDTVA  = GZDTVA (ILAYER)
      IF (LDTVA.GT.0) GZDNLI = LC (LDTVA-IZDNLI)

  999 RETURN
      END
