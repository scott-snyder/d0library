      SUBROUTINE CLNPH(VTX,DIR,PHI,NS,S)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Finds intersection of line with phi plane
C-
C-   Inputs  : VTX(3) A point on the line
C-             DIR(3) The direction cosines of the line
C-             PHI    The azimuth of the plane
C-   Outputs : NS     The number of s-values returned (0 or 1)
C-             S      The arc length along the line to the intersection
C-   Controls:
C-
C-   Created  24-JUL-1989   Michael W. Peters
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      REAL VTX(3),DIR(3),PHI
      INTEGER NS
      REAL S
      DOUBLE PRECISION TANPHI,DEN
C
      TANPHI=TAN(PHI)
      DEN=DIR(2)-DIR(1)*TANPHI
      IF(DEN.EQ.0.) THEN
        NS=0
      ELSE
        S=(VTX(1)*TANPHI-VTX(2))/DEN
        NS=1
      ENDIF
  999 RETURN
      END
