      LOGICAL FUNCTION SAMPAR()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read the muon database parameters
C-
C-   Returned value  :
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  23-DEC-1991   Daria Zieminska 
C-   Updated  25-MAY-1994   Andrei Mayorov  force call to MCONST to accurete 
C-                          map field
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL SCONST
      INTEGER FLG,FLAG_MUD1
      EXTERNAL FLAG_MUD1
C set flag to force update of constants on next call to MCONST
      FLG = FLAG_MUD1(1)
c
      CALL EZPICK('SAMRECO_RCP')
      SAMPAR = SCONST()
      CALL EZRSET
C
  999 RETURN
      END
