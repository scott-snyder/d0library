      SUBROUTINE PDALGN(x,y,z)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : convert (X,Y,Z) to the survey system
C-
C-   Inputs  : X,Y,Z: old coordinators of the point to be converted
C-   Outputs : X,Y,Z: new coordinators of the point to be converted
C-
C-   Created  10-APR-1991   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    MATRIX(3,3), VECTOR(3), X, Y, Z, X0, Y0, Z0
      DATA    MATRIX/0.9952,-0.09797,0.0002291,0.09797,0.9952,
     &               -0.0002292, -0.0002056,0.0002505,1.0/
      DATA VECTOR/0.00594,0.01382,-0.00673/
C----------------------------------------------------------------------
C
      X0 = X
      Y0 = Y
      Z0 = Z
      X = matrix(1,1)*x0 + matrix(1,2)*y0 + matrix(1,3)*z0
     &                  + vector(1)
      Y = matrix(2,1)*x0 + matrix(2,2)*y0 + matrix(2,3)*z0
     &                  + vector(2)
      Z = matrix(3,1)*x0 + matrix(3,2)*y0 + matrix(3,3)*z0
     &                  + vector(3)
C
  999 RETURN
      END
