      SUBROUTINE MUPLNZX(DY,X,DIRC,XH,IHIT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Finds intercept of muon track and a plane
C-                         parallel to XZ plane
C-
C-   Inputs  : DY     : Y of plane
C-             X(3)   : Track origin
C-             DIRC   : Direction cosines
C-
C-   Outputs : XH(3)  : Hit point
C-   Controls: IHIT   : If 0 no hit
C-
C-   Created  29-OCT-1991   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL X(3),DIRC(3),XH(3)
      INTEGER IHIT
C
      REAL EPS, RHO, DY
      DATA EPS /1.0E-6/
C
      IHIT = 1
C
      IF(ABS(DIRC(2)) .LT. EPS) THEN
        IHIT = 0
        GOTO 999
      ENDIF
C
      RHO = (DY - X(2)) / DIRC(2)
      XH(1) = X(1) + RHO * DIRC(1)
      XH(2) = DY
      XH(3) = X(3) + RHO * DIRC(3)
C
C----------------------------------------------------------------------
  999 RETURN
      END
