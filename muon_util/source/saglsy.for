C+
      SUBROUTINE SAGLSY (R1, SYS, R2)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Move point R1 to new coordinate system.
C-
C-   Inputs  : R1(3) - vector in the old coordinate system,
C-             SYS(6) - parameters of the new coordinates system.
C-   Outputs : R2(3) - vector in the new coordinate system.
C-   Controls: none.
C-
C-   Created  16-NOV-1992   Alexander Efimov   
C-   Updated  20-NOV-1992   Alexander Efimov   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      REAL*8  R1(3), SYS(6), R2(3)
      REAL*8  S, C, X, Y
      INTEGER J
C
      DO J = 1, 3
        R2(J) = R1(J)
      END DO
      S = SIN (SYS(4))
      C = COS (SYS(4))
      X =  R2(1) * C + R2(2) * S
      Y = -R2(1) * S + R2(2) * C
      R2(1) = X
      R2(2) = Y
      S = SIN (SYS(5))
      C = COS (SYS(5))
      X =  R2(2) * C + R2(3) * S
      Y = -R2(2) * S + R2(3) * C
      R2(2) = X
      R2(3) = Y
      S = SIN (SYS(6))
      C = COS (SYS(6))
      X =  R2(1) * C + R2(2) * S
      Y = -R2(1) * S + R2(2) * C
      R2(1) = X
      R2(2) = Y
      DO J = 1, 3
        R2(J) = R2(J) + SYS(J)
      END DO
C
      RETURN
      END
