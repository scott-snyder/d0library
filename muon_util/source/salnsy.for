C+
      SUBROUTINE SALNSY (A, U, P, OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Solving system of the linear equations
C-                         by the help of Gauss method.
C-
C-   Inputs  : A - linear system matrix,
C-             U - linear system dimension.
C-   Outputs : P - linear system solution.
C-   Controls: OK = -1 : no solution - singular matrix
C-             OK = +1 : solution is found
C-
C-   Created  17-MAY-1992   Alexander Efimov
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INTEGER U, UP1
      REAL*8 A(U,U+1), P(U), T
      INTEGER OK
      INTEGER K, N, I, J
C
      OK = -1
      UP1 = U + 1
      DO N = 1, U
        DO I = N, U
          IF (ABS(A(I,N)) .GT. 0.0) THEN
            K = I
            GOTO 1
          END IF
        END DO
C
C       singular matrix
C
        RETURN
C
    1   CONTINUE
        IF (K .NE. N) THEN
          DO I = N, UP1
            T = A(N,I)
            A(N,I) = A(K,I)
            A(K,I) = T
          END DO
        END IF
        DO J = UP1, N, -1
          A(N,J) = A(N,J)/A(N,N)
        END DO
        DO I = K+1, U
          DO J = N+1, UP1
            A(I,J) = A(I,J) - A(I,N)*A(N,J)
          END DO
        END DO
      END DO
      DO I = U, 1, -1
        P(I) = A(I,UP1)
        DO J = I-1, 1, -1
          A(J,UP1) = A(J,UP1) - A(J,I)*P(I)
        END DO
      END DO
C
      OK = +1
      RETURN
      END
