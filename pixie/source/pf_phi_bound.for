      SUBROUTINE PF_PHI_BOUND
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Action routine to display the hits, segments
C-                         and tracks in two FDC sectors (cell), 
C-                         at boundary.
C-      
C-   Created  11-JUN-1991   Robert E. Avery
C-   Updated  19-FEB-1992   Robert E. Avery  Put dialogue into  PFPICK_SECTOR
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER HALF,QUAD,SECTOR
      DATA HALF,QUAD,SECTOR/ 0,0,0/
C----------------------------------------------------------------------
C
      CALL PFPICK_SECTOR(1,HALF,QUAD,SECTOR)
      IF (HALF .EQ. 2) GOTO 999
C
      CALL PF_PHI_BOUND_VIEW(HALF,SECTOR)
C
C----------------------------------------------------------------------
  999 RETURN
      END
