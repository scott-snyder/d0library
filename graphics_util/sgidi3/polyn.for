      SUBROUTINE POLYN(NS,POINTS)
C-------------------------------------------------------------------
C-
C-   Pourpose and Methods: This routine performs the drawing of a 
C-   polygone.
C-
C-   Inputs: NS      [I ]: Number of points in the polygone
C-           POINTS[I*,*]: Array containing the points of the 
C-                         polygone.
C-
C-   Outputs: None
C-  
C-   Created 24-AUG-1992 Mike Shupe
C-
C-------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER NS
      INTEGER POINTS(3,*)
C
      INTEGER I
C-------------------------------------------------------------------
C
C *** Move to the starting point
C
      CALL MOVE(POINTS(1,1),POINTS(2,1),POINTS(3,1))
C
C *** Draw the points
C
      DO I=2, NS
        CALL DRAW(POINTS(1,I),POINTS(2,I),POINTS(3,I))
      ENDDO
      RETURN
      end
