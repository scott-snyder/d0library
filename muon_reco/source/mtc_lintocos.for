      SUBROUTINE MTC_LINTOCOS( ZX,SMX, ZY,SMY, POINT, COSDIR)
C----------------------------------------------------------------------
C- MTC_LINTOCOS: part of MTC (Muon Tracking in the Calorimeter) package
C-
C-   Purpose and Methods : turn the 2 input slopes and intercepts
C-      defining planes in the xz and yz planes into a
C-      3-D line defined by a point and a set of direction cosines.
C-      point
C-
C-   Inputs  : ZX  - z intercept in xz plane
C-             SMX - slope of projection onto xz plane
C-             ZY  - z intercept in yz plane
C-             SMY - slope on projection onto yz plane
C-   Outputs : POINT(3)  - the coordinates of a point on the line
C-              where the input planes intersect
C-             COSDIR(3) - direction cosines of the line where
C-              the input planes intersect
C-
C-   Created   6-OCT-1993   Elizabeth Gallas - reference notebook II-62
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C- input ...
      REAL ZX,SMX, ZY,SMY
C- output ...
      REAL POINT(3), COSDIR(3)
C- local ...
      REAL ZXY, SMXY
      REAL SMX2,SMY2,SUM
C----------------------------------------------------------------------
C- Get the first point on the line, but make sure no slopes are infinity ...
      IF(SMX.NE.0. .AND. SMY.NE.0.) THEN
        POINT(1) = -ZX/SMX
        POINT(2) = -ZY/SMY
        POINT(3) = 0.
      ELSE IF(SMX.EQ.0.) THEN
        POINT(1) = 0.
        POINT(2) = (ZX-ZY)/SMY
        POINT(3) = ZX
      ELSE IF(SMY.EQ.0.) THEN
        POINT(1) = (ZX-ZY)/SMX
        POINT(2) = 0.
        POINT(3) = ZY
      ELSE
        WRITE(6,*) ' MTC_LINTOCOS:  error !!! both input slopes zero '
      END IF
C- Now get the direction cosines ...
      SMX2 = SMX**2
      SMY2 = SMY**2
      SUM  = SQRT(SMX2 + SMY2 + SMX2*SMY2)
      COSDIR(1) = SMY / SUM
      COSDIR(2) = SMX / SUM
      COSDIR(3) = SMX * SMY / SUM

  999 RETURN
      END
