      SUBROUTINE GMD_DRAW_RECO
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draw RECO Objects.
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  11-DEC-1991   Harrison B. Prosper
C-   Updated   6-MAY-1993   Harrison B. Prosper
C-   Updated  25-MAY-1993   Marc Paterno  Corrected FLINT complaints
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GM_4VECT.INC'
C----------------------------------------------------------------------
      CALL GMD_DRAW_GRID
      CALL GMD_DRAW_MANY_OBJECTS(NPART+1,NOBJECT)
      RETURN
      END
