      SUBROUTINE RFLROT(ROT, IAXIS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      To reflect the Rotation Matrix by rotation of 180 degrees 
C-      around an axis specified by IAXIS.
C-   Inputs  :   ROT       rotation matrix (REAL*8 3x3 array)
C-               IAXIS     axis of rotation 1:x, 2:y, 3:z
C-   Outputs :   ROT       reflected rotation matrix
C-   Error Stop Code:      998      invalid axis code
C-
C-   Created   2-MAR-1988   Stephen Kahn, Esq.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL*8 ROT(3, 3)
      INTEGER IAXIS, I, J, K
C----------------------------------------------------------------------
      IF (IAXIS .LE.0 .OR. IAXIS .GE.4) THEN
         WRITE(6, 10) IAXIS
   10    FORMAT(' RFLROT: INVALID AXIS CODE ',I5)
         STOP 988
      END IF
      DO 100 I = 1, 3
      DO 100 J = 1, 3
         IF ( J.NE. IAXIS) THEN
            ROT(I, J) = - ROT(I, J)
         END IF
  100 CONTINUE
C
  999 RETURN
      END
