      SUBROUTINE PVPHI_ROAD
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set up to draw phi road limit for VTX
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
      INTEGER IDET,VTXROAD
      DATA IDET/2/
C----------------------------------------------------------------------
      CALL PUGETV('VTX DRAW ROAD',VTXROAD)
      IF(VTXROAD.GT.0) CALL PZPHI_ROAD(IDET)
  999 RETURN
      END
