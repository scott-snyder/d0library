      SUBROUTINE PMPICK
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Checks the input from the user to see if 
C-   the point selected belongs in the muon detector to display legends
C-
C-   Inputs  : None
C-   Outputs : None
C-
C-   Created   4-NOV-1991   Lupe Howell
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      REAL    COORD(3)
C
      REAL    MUON_XMIN
      PARAMETER( MUON_XMIN = 428.6 )
C----------------------------------------------------------------------
C
C ****  Get window coordinates of picked point
C
      CALL PU_GET_PICKW(COORD)
C
C ****  Check if coordenates entered in muon
C
      IF( (COORD(1) .GE. MUON_XMIN) .OR. 
     &    (COORD(1) .LE. -MUON_XMIN) )THEN
        CALL PMLEGEND
      ENDIF
  999 RETURN
      END
