      SUBROUTINE MUIMPP(V,X,C,R)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Finds impact parameter of track at
C-                         vertex point
C-
C-   Inputs  : V(3)    : Vertex coordinates
C-             X(1:3)  : Track origin coordinate
C-             X(4:6)  : Track direction cosines
C-   Outputs : 
C-               C(3)  : Coordinates of closest point of approach
C-               R     : Impact parameter
C-   Controls: 
C-
C-   Created  31-OCT-1991   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL V(3),X(6),C(3),R
      REAL RHO
C
      RHO = X(4) * (V(1) - X(1)) + X(5) * (V(2) - X(2)) +
     &            X(6) * (V(3) - X(3))
      C(1) = X(1) + RHO * X(4)
      C(2) = X(2) + RHO * X(5)
      C(3) = X(3) + RHO * X(6)
C
      R = SQRT( (V(1) - C(1))**2 + 
     &           (V(2) - C(2))**2 + (V(3) - C(3))**2 )
C
C----------------------------------------------------------------------
  999 RETURN
      END
