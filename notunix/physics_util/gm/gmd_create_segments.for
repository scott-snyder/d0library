      SUBROUTINE GMD_CREATE_SEGMENTS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create a segment for each 4-vector object.
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  11-DEC-1991   Harrison B. Prosper
C-   Updated   6-MAY-1993   Harrison B. Prosper
C-   Updated   8-MAY-1993   Harrison B. Prosper
C-   Updated  25-MAY-1993   Marc Paterno  Correct FLINT complaints
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:GM_4VECT.INC'
      LOGICAL DRAW_ALLCONES, DRAW_ALLPNUTS
C----------------------------------------------------------------------
C
C ****  Draw ALL segments
C
      CALL PUGETV('DRAW ALLCONES',DRAW_ALLCONES)
      CALL PUGETV('DRAW ALLPNUTS',DRAW_ALLPNUTS)
C
      CALL PUSETV('DRAW ALLCONES',.TRUE.)
      CALL PUSETV('DRAW ALLPNUTS',.TRUE.)
C
C ****  Draw particles
C
      CALL GMD_DRAW_GRID
      CALL JDVISB(0)   ! Segments NOT to be displayed
      CALL GMD_DRAW_MANY_OBJECTS(1,NOBJECT)
      CALL JDVISB(1)   ! Segments VISIBLE
C
C ****  Reset draw_allcones, draw_allpnuts
C
      CALL PUSETV('DRAW ALLCONES',DRAW_ALLCONES)
      CALL PUSETV('DRAW ALLPNUTS',DRAW_ALLPNUTS)
C
      RETURN
      END
