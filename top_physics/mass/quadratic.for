      SUBROUTINE QUADRATIC(A,NSOL,SOL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : SOLVE A QUADRATIC EQUATION
C-   A(1)*X**2 + A(2)X + A(3) = 0
C-
C-   Inputs  : A
C-   Outputs : NSOL = NUMBER OF SOLUTIONS
C-   SOL(2) SOLUTIONS

C-   Controls: 
C-
C-   Created  17-MAR-1993   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    A(*)
      INTEGER NSOL
      REAL    SOL(2), ROOT
C----------------------------------------------------------------------
      ROOT = A(2)*A(2) - 4.*A(1)*A(3)
      IF ( ROOT.LT.0.0 ) THEN
        NSOL = 0
      ELSE
        ROOT = SQRT(ROOT)
        NSOL = 2
        SOL(1) = (-A(2)-ROOT)/(2.0*A(1))
        SOL(2) = (-A(2)+ROOT)/(2.0*A(1))
      ENDIF
  999 RETURN
      END
