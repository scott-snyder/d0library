      SUBROUTINE DOT(A,B,NDIM,C)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculates the Dot product of Vectors of A,B of
C-   dimension NDIM and outputs it as C
C-
C-   Inputs  : A,B,NDIM
C-   Outputs : C
C-   Controls: 
C-
C-   Created   9-JAN-1991   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    A(*),B(*),C
      INTEGER NDIM,I
C----------------------------------------------------------------------
      C = 0.
      DO 10 I = 1 ,NDIM  
        C = C + A(I)*B(I)
   10 CONTINUE
C
  999 RETURN
      END
