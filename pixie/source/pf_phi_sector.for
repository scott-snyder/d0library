      SUBROUTINE PF_PHI_SECTOR
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Action routine to display the hits, segments
C-                         and tracks in an FDC sector (cell).
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  29-APR-1991   Robert E. Avery
C-   Updated  14-MAY-1991   Susan K. Blessing  Use North and South halves 
C-   Updated  15-MAY-1991   Robert E. Avery   Call PF_PHI_SECT_VIEW to do
C-                                              the real work.
C-   Updated  19-FEB-1992   Robert E. Avery  Put dialogue into  PFPICK_SECTOR
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
      CALL PF_PHI_SECT_VIEW(HALF,SECTOR)
C
C----------------------------------------------------------------------
  999 RETURN
      END
