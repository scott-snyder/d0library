C+
      REAL FUNCTION SALINE (R1, R2, V1, V2, P1, P2)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : find distance between two lines in space.
C-
C-   Returned value  : distance between lines
C-   Inputs  : R1(3) - point on the first line
C-             R2(3) - point on the second line
C-             V1(3) - vector along the first line
C-             V2(3) - vector along the second line
C-
C-   Outputs : P1(3) - point on the first line at minimum distance
C-             P2(3) - point on the second line at minimum distance
C-   Controls: 
C-
C-   Created  20-SEP-1990   A. Efimov
C-   Updated   3-MAY-1991   Vladimir Glebov : Change name  
C-   Updated  25-MAR-1992   Daria Zieminska  introduce SMALL 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL R1(3), R2(3), V1(3), V2(3), P1(3), P2(3)
      REAL A, B, C, D, Q, T1, T2,SMALL
      INTEGER J
      DATA SMALL/1.0E-5/
C
      A = 0.0
      B = 0.0
      C = 0.0
      DO J = 1, 3
        Q = R1(J) - R2(J)
        A = A + V1(J) * V2(J)
        B = B + Q * V1(J)
        C = C + Q * V2(J)
      END DO
      D = 1.0 - A * A
      IF (D .GT. SMALL ) THEN
        T1 = (A * C - B) / D
        T2 = (C - A * B) / D
      ELSE
        T1 = 0.0
        T2 = B
      END IF
      D = 0.0
      DO J = 1, 3
        P1(J) = R1(J) + T1 * V1(J)
        P2(J) = R2(J) + T2 * V2(J)
        D = D + (P1(J) - P2(J)) ** 2
      END DO
      SALINE = SQRT(D)
C
      RETURN
      END
