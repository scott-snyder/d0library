C+
      SUBROUTINE SACR4L (L1, L2, L3, L4, LINE, OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Find track parameters (LINE) space which
C-                         cross 4 lines (L1,L2,L3,L4) in 3-dimensional
C-                         space.
C-
C-   Inputs  : L1,L2,L3,L4(1:6) - lines parameters.
C-   Outputs : LINE(1:6) - track parameters
C-   Controls: OK = -1 : no such track
C-             OK = +1 : track parameters are calculated
C-
C-   Created  17-MAY-1992   Alexander Efimov
C-   Updated   3-JUN-1992   Alexander Efimov   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      REAL    L1(6), L2(6), L3(6), L4(6), LINE(6)
      INTEGER OK
      REAL*8  M44(4,5), P4(4), Q
C
      OK = -1
      M44(1,1) = + L1(5)
      M44(1,2) = - L1(4)
      M44(1,3) = + L1(3) * L1(5)
      M44(1,4) = - L1(3) * L1(4)
      M44(1,5) = L1(1) * L1(5) - L1(2) * L1(4)
      M44(2,1) = + L2(5)
      M44(2,2) = - L2(4)
      M44(2,3) = + L2(3) * L2(5)
      M44(2,4) = - L2(3) * L2(4)
      M44(2,5) = L2(1) * L2(5) - L2(2) * L2(4)
      M44(3,1) = + L3(5)
      M44(3,2) = - L3(4)
      M44(3,3) = + L3(3) * L3(5)
      M44(3,4) = - L3(3) * L3(4)
      M44(3,5) = L3(1) * L3(5) - L3(2) * L3(4)
      M44(4,1) = + L4(5)
      M44(4,2) = - L4(4)
      M44(4,3) = + L4(3) * L4(5)
      M44(4,4) = - L4(3) * L4(4)
      M44(4,5) = L4(1) * L4(5) - L4(2) * L4(4)
      CALL SALNSY (M44, 4, P4, OK)
      IF (OK .GT. 0) THEN
        LINE(1) = P4(1)
        LINE(2) = P4(2)
        LINE(3) = 0.0
        Q = 1.0 / SQRT (P4(3)**2 + P4(4)**2 + 1.0)
        LINE(4) = P4(3) * Q
        LINE(5) = P4(4) * Q
        LINE(6) = Q
      END IF
C
      RETURN
      END
