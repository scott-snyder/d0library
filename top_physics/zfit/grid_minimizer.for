C------------------------------------------------------------------------
      REAL*8 FUNCTION GRID_MINIMIZER(X0,X1,DX,FCN,XMIN)
C-----------------------------------------------------------------------
C-
C-   Purpose and Methods: Find the minimum value of a function evaluated
C-     at a set of fixed grid points.
C-
C-   Inputs  : X0     - Lower bound of range to scan
C-             X1     - Upper bound of scan range
C-             DX     - Increment
C-             FCN    - Function to evaluate
C-   Outputs :
C-   Controls:
C-
C-   Created  27-Apr-1994   John D. Hobbs
C-
C-----------------------------------------------------------------------
      IMPLICIT NONE
      REAL*8 X0,X1,DX,FCN,XMIN,X
      REAL*8 FVAL,FMIN
      EXTERNAL FCN
C-----------------------------------------------------------------------
      FMIN=1.0E30
      XMIN=1.0E10
      DO X=X0,X1,DX
        FVAL=FCN(X)
        IF( FVAL.LT.FMIN ) THEN
          XMIN=X
          FMIN=FVAL
        ENDIF
      ENDDO
      GRID_MINIMIZER=FMIN
  999 RETURN
      END
