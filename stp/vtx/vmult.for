      SUBROUTINE VMULT(MAT,V)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : V --> MAT*V
C-
C-   Inputs  : V    Input vector
C-             MAT  matrix
C-   Outputs : V    MAT*V
C-   Controls: 
C-
C-   Created  12-AUG-1992   Ed Oltman
C-   Updated  12-FEB-1993   Ed Oltman  FOR D0 SOFTWARE
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
c I/O:
      DOUBLE PRECISION MAT(3,3),V(3)
c Locals:
      DOUBLE PRECISION VV(3)
      INTEGER I,J
C----------------------------------------------------------------------
      DO I = 1,3
        VV(I) = V(I)
        V(I)  = 0.
      ENDDO
      DO I = 1,3
        DO J = 1,3
          V(I) = V(I) + MAT(I,J)*VV(J)
        ENDDO
      ENDDO
  999 RETURN
      END
