      SUBROUTINE CROSS(A,B,C)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : gives the cross product of vectors A and B
C-    into vector C
C-
C-   Inputs  : A,B
C-   Outputs : C
C-   Controls: 
C-
C-   Created   9-JAN-1991   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    A(*),B(*),C(*)
C----------------------------------------------------------------------
      C(1) = A(2)*B(3) - A(3)*B(2)
      C(2) = B(1)*A(3) - B(3)*A(1)
      C(3) = A(1)*B(2) - A(2)*B(1)
  999 RETURN
      END
