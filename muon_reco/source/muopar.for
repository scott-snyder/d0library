      LOGICAL FUNCTION MUOPAR()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read the muon database parameters
C-
C-   Returned value  :
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  18-APR-1991   SHAHRIAR ABACHI
C-   DH 12/91: CALL MCONST SO EXAMINE/D0USER THE SAME
C-   Updated  30-DEC-1991   Daria Zieminska   call SAMPAR
C-   Modified 11-MAR-1993   Alexander Kozelov  Take into account SAMPAR
C-                                             return value
C-   Modified 21-DEC-1993 D.Wood remove call to MCONST;instead use FLAG_MUD1
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL MCONST,SAMPAR
      INTEGER FLG,FLAG_MUD1
      EXTERNAL FLAG_MUD1
C set flag to force update of constants on next call to MCONST
      FLG = FLAG_MUD1(1)
      MUOPAR = SAMPAR()
C
  999 RETURN
      END
