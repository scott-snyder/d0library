      SUBROUTINE PFTHETA_ROAD
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set up to draw theta road limits for FDC
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   6-SEP-1991   Sharon Hagopian
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
C----------------------------------------------------------------------
      INTEGER IDET,FDCROAD
      DATA IDET/3/
C----------------------------------------------------------------------
      CALL PUGETV('FDC DRAW ROAD',FDCROAD)
      IF(FDCROAD.GT.0) CALL PZTHETA_ROAD(IDET)
  999 RETURN
      END
