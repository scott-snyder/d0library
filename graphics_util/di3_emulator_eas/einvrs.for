      SUBROUTINE EINVRS(MX, MXI)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To inverse a 4X4 matrix.
C-
C-   Inputs  : MX ; Input matrix.
C-   Outputs : MXI  ; Output matrix as inverse of MX
C-   Controls: NONE
C-
C-   Created   2-JUL-1990   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
        REAL MX(4,4), MXI(4,4)
        REAL MUNIT(4,4)
        INTEGER I,J
C
      DO I=1,4
        DO J=1,4
          MUNIT(I,J)=0.0
          IF(I .EQ. J) MUNIT(I,J)=1.0
        ENDDO
      ENDDO
C
      DO I=1,4
        DO J=1,4
          MXI(I,J) = MX(I,J)
        ENDDO
      ENDDO
C
      CALL MXEQU(MXI, MUNIT, 4, 4)
C
      DO I=1,4
        DO J=1,4
          MXI(I,J) = MUNIT(I,J)
        ENDDO
      ENDDO
C
 999  CONTINUE
      RETURN
      END
