      SUBROUTINE GMD_DRAW_FULL_EVENT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draw all objects.
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  11-DEC-1991   Harrison B. Prosper
C-   Updated   6-MAY-1993   Harrison B. Prosper
C-   Updated  25-MAY-1993   Marc Paterno  Correct FLINT complaints.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:GM_4VECT.INC'
C----------------------------------------------------------------------
      CALL GMD_DRAW_GRID
      CALL GMD_DRAW_MANY_OBJECTS(1,NOBJECT)
      RETURN
      END
