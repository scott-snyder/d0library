C+
      SUBROUTINE SADSPL (P, L, DIST2, Q)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculate distance between point and line
C-                         in 3-dimensional space.
C-
C-   Inputs  : P(3) - coordinates of the point,
C-             L(6) - line parameters.
C-   Outputs : DIST2 - distance between point and line,
C-             Q - distatance from point L(1:3) to the point on on
C-                  the line with minimum distance,
C-   Controls: none.
C-
C-   Created  17-JUN-1992   Alexander Efimov
C-   Updated  19-AUG-1992   Alexander Efimov   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      REAL    P(3), L(6), DIST2, Q
      INTEGER OK
      REAL*8  A, D2, QQ
      INTEGER J
C
      QQ = 0.0
      DO J = 1, 3
        QQ = QQ + (P(J) - L(J)) * L(J+3)
      END DO
      D2 = 0.0
      DO J = 1, 3
        A = L(J) + QQ * L(J+3) - P(J)
        D2 = D2 + A * A
      END DO
      DIST2 = D2
      Q = QQ
C
      RETURN
      END
