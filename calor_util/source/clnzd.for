      SUBROUTINE CLNZD(VTX,DIR,ZED,NS,S)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find intersection of a line with a z-plane
C-
C-   Inputs  : VTX(3)  A point on the line
C-             DIR(3)  Direction cosines of the line
C-             ZED     Z of the plane
C-   Outputs : NS      Number of s-values returned (0 or 1)
C-             S       Arc length along the line to the intersection
C-   Controls:
C-
C-   Created  24-JUL-1989   Michael W. Peters
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      REAL VTX(3),DIR(3),ZED
      INTEGER NS
      REAL S
C
      IF(DIR(3).EQ.0.) THEN
        NS=0
      ELSE
        S=(ZED-VTX(3))/DIR(3)
        NS=1
      ENDIF
  999 RETURN
      END
