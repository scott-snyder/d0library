      SUBROUTINE MUPLNYZ(DX,X,DIRC,XH,IHIT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Finds intercept of muon track and plane
C-                         parallel to XY plane
C-
C-   Inputs  : DX     : Z of plane
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
      REAL EPS, RHO, DX
      DATA EPS /1.0E-6/
C
      IHIT = 1
C
      IF(ABS(DIRC(1)) .LT. EPS) THEN
        IHIT = 0
        GOTO 999
      ENDIF
C
      RHO = (DX - X(1)) / DIRC(1)
      XH(1) = DX
      XH(2) = X(2) + RHO * DIRC(2)
      XH(3) = X(3) + RHO * DIRC(3)
C
C----------------------------------------------------------------------
  999 RETURN
      END
