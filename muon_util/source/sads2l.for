C+
      SUBROUTINE SADS2L (L1, L2, DIST2, Q1, Q2, OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculate distance between two lines
C-                         in 3-dimensional space.
C-
C-   Inputs  : L1(6) - 1-st line,
C-             L2(6) - 2-nd line.
C-   Outputs : DIST2 - distance between lines,
C-             Q1 - distatance form point L1(1:3) to the point on on
C-                  the 1-st line with minimum distance,
C-             Q2 - distatance form point L1(1:3) to the point on on
C-                  the 2-nd line with minimum distance,
C-   Controls: OK = -1 : distance can't be calculated,
C-                = +1 : distance is calculated.
C-
C-   Created  31-MAR-1992   Alexander Efimov
C-   Updated   8-OCT-1993   Alexander Efimov   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      REAL    L1(6), L2(6), DIST2, Q1, Q2
      DOUBLE PRECISION  A0, B0, B1, B2, Q, R1, R2, R3, D2
      DOUBLE PRECISION  DL1(6), DL2(6)
      INTEGER OK, J
C
      OK = -1
      DO J = 1, 6
        DL1(J) = L1(J)
        DL2(J) = L2(J)
      END DO
      A0 = DL1(4) * DL2(4) + DL1(5) * DL2(5) + DL1(6) * DL2(6)
      Q = A0 * A0 - 1.0
      IF (ABS(Q) .LT. 1.0E-13) RETURN
      R1 = DL2(1) - DL1(1)
      R2 = DL2(2) - DL1(2)
      R3 = DL2(3) - DL1(3)
      B0 = R1 * R1 + R2 * R2 + R3 * R3
      B1 = R1 * DL1(4) + R2 * DL1(5) + R3 * DL1(6)
      B2 = R1 * DL2(4) + R2 * DL2(5) + R3 * DL2(6)
      Q1 = (A0 * B2 - B1) / Q
      Q2 = (B2 - A0 * B1) / Q
      D2 = B0 + (Q1 - 2.0 * B1) * Q1 + 
     &     (2.0 * (B2 - A0 * Q1) + Q2) * Q2
      DIST2 = ABS(D2)
      OK = +1
C
      RETURN
      END
