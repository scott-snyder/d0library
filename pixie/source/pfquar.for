      SUBROUTINE PFQUAR( HALF, QUART, XMIN, XMAX, YMIN, YMAX )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Draw display of one quarter of FDC Half 
C-                         (forward/backward) with hits and tracks,
C-                         if requested
C-
C-   Inputs  : HALF, QUART = half, quarter of FDC
C-             XMIN, XMAX  = minimum and maximum x-width of display
C-             YMIN, YMAX  = minimum and maximum y-height of display
C-   Outputs : display of one quarter 
C-   Controls: 
C-
C-   Created  21-OCT-1988   Jeffrey Bantly
C-   Updated  16-MAR-1990   Jeffrey Bantly  use logical format 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER HALF, QUAD, QUART
      REAL    XMIN, XMAX, YMIN, YMAX
C----------------------------------------------------------------------
C   draw quarter
C
C   draw theta on display left
C
      QUAD = QUART
      CALL PFTHT4( HALF, QUAD, XMIN, XMAX, YMIN, YMAX )
C
C   draw phi on display
C
      CALL PFPHI4( HALF, QUART, XMIN, XMAX, YMIN, YMAX )
C
C   draw theta on display right
C
      QUAD = QUART+4
      CALL PFTHT4( HALF, QUAD, XMIN, XMAX, YMIN, YMAX )
C
C----------------------------------------------------------------------
  999 RETURN
      END
