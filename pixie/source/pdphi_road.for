      SUBROUTINE PDPHI_ROAD
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set up to draw phi road limit for CDC
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
      INTEGER IDET,CDCROAD
      DATA IDET/1/
C----------------------------------------------------------------------
      CALL PUGETV('CDC DRAW ROAD',CDCROAD)
      IF(CDCROAD.GT.0) CALL PZPHI_ROAD(IDET)
  999 RETURN
      END
