      SUBROUTINE MUPLNXY(DZ,X,DIRC,XH,IHIT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Finds intercept of muon track and plane
C-                         parallel to XY plane
C-
C-   Inputs  : DZ     : Z of plane
C-             X(3)   : Track origin
C-             DIRC   : Direction cosines
C-
C-   Outputs : XH(3)  : Hit point
C-   Controls: IHIT   : If 0 no hit
C-
C-   Created  30-OCT-1991   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL X(3),DIRC(3),XH(3)
      INTEGER IHIT
C
      REAL EPS, RHO, DZ
      DATA EPS /1.0E-6/
C
      IHIT = 1
C
      IF(ABS(DIRC(3)) .LT. EPS) THEN
        IHIT = 0
        GOTO 999
      ENDIF
C
      RHO = (DZ - X(3)) / DIRC(3)
      XH(1) = X(1) + RHO * DIRC(1)
      XH(2) = X(2) + RHO * DIRC(2)
      XH(3) = DZ
C
C----------------------------------------------------------------------
  999 RETURN
      END
