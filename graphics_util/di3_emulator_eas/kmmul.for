      SUBROUTINE KMMUL(MAT1, MAT2, MATR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Multiplies two 4X4 matrices.
C-
C-   Inputs  : MAT1(4,4), MAT2(4,4)
C-   Outputs : MATR(4,4)
C-   Controls: 
C-
C-   Created  20-OCT-1989   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL MAT1(4,4), MAT2(4,4), MAT3(4,4), MATR(4,4)
      INTEGER I, J, K

      DO 10 I=1,4
         DO 10 J=1,4
            MAT3(I,J) = 0.0
            DO 10 K=1,4
               MAT3(I,J) = MAT3(I,J) + MAT1(I,K) * MAT2(K,J)
   10 CONTINUE
      MAT3(4,4) = 1.0
      DO 20 I=1,4
         DO 20 J=1,4
            MATR(I,J) = MAT3(I,J)
   20 CONTINUE
C
  999 RETURN
      END
