C+
      SUBROUTINE SACRPL (P, L1, L2, LINE, OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find parameters of the track (LINE) which
C-                         cross point and 2 lines (L1,L2)
C-                         in 3-dimensional space.
C-                         (it is assumed that L1(6) and L2(6) = 0.0)
C-
C-   Inputs  : P(1:3) - point coordinates,
C-             L1,L2(1:6) - lines parameters.
C-   Outputs : LINE(1:6) - track parameters
C-   Controls: OK = -1 : no such track
C-             OK = +1 : track parameters are calculated
C-
C-   Created   3-JUN-1992   Alexander Efimov   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      REAL    P(3), L1(6), L2(6), LINE(6)
      INTEGER OK
      REAL*8  Q, Q1, Q2, G1, G2
      INTEGER J
C
      OK = -1
      DO J = 1, 3
        LINE(J) = P(J)
      END DO
      Q1 = L1(3) - P(3)
      Q2 = L2(3) - P(3)
      IF (ABS(Q1) .LT. 1.0E-17) RETURN
      IF (ABS(Q2) .LT. 1.0E-17) RETURN
      G1 = L1(4) * (P(2) - L1(2)) - L1(5) * (P(1) - L1(1))
      G2 = L2(4) * (P(2) - L2(2)) - L2(5) * (P(1) - L2(1))
      G1 = G1 / Q1
      G2 = G2 / Q2
      Q = L1(4) * L2(5) - L2(4) * L1(5)
      IF (ABS(Q) .LT. 1.0E-17) RETURN
      Q1 = L1(4) * G2 - L2(4) * G1
      Q2 = L1(5) * G2 - L2(5) * G1
      Q1 = Q1 / Q
      Q2 = Q2 / Q
      Q = 1.0 / SQRT (Q1 * Q1 + Q2 * Q2 + 1.0)
      LINE(4) = Q1 * Q
      LINE(5) = Q2 * Q
      LINE(6) = Q
      OK = +1
C
      RETURN
      END
