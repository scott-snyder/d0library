      SUBROUTINE QUARTIC(A,NSOL,SOL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : FINDS THE SOLUTIONS OF A QUARTIC EQUATION
C-   X**4 + A(1)*X**3 + A(2)*X**2 + A(3)*X + A(4) = 0
C-
C-   Inputs  : A
C-   Outputs : NSOL = NUMBER OF SOLUTIONS . SHOULD BE = 0,2, OR 4
C-             SOL(4) = SOLUTIONS
C-   Controls: 
C-
C-   Created  17-MAR-1993   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    A(*),SOL(*)
      INTEGER NSOL
      REAL    P,Q,R,S
      REAL    B(4),DEL
      COMPLEX Z1,Z2,Z3
      REAL    AA,BB,K
      INTEGER NSOLQ
      REAL    SOLQ(2),C(3)
C----------------------------------------------------------------------
      P = A(1)
      Q = A(2)
      R = A(3)
      S = A(4)   !FOR EASE OF DEBUGGING
C
      B(1) = 1.0  !RESOLVENT CUBIC EQUATION
      B(2) = -Q/2.0
      B(3) = (P*R-4*S)/4
      B(4) = (4*Q*S-P*P*S-R*R)/8.
C
      CALL CUBIC(B,Z1,Z2,Z3)
C
      DEL = AIMAG(Z1)
      IF ( ABS(DEL).GT..001 ) THEN
C
C ****  IMAGINARY ROOT
C
        NSOL = 0
        RETURN
      ELSE
        K = Z1
        AA = (2*K + P*P/4) - Q
        IF ( AA.LT.0.0 ) THEN
          NSOL = 0
          RETURN
        ELSE
          AA = SQRT(AA)
        ENDIF
        BB = (K*P-R)/(2.*AA)
        C(1) = 1.0
        C(2) = (P/2) - AA
        C(3) = K -BB
C SOLVE QUADRATIC EQUATION 
        CALL QUADRATIC(C,NSOLQ,SOLQ)
        NSOL = 0
        IF ( NSOLQ.GT.0 ) THEN
          CALL UCOPY(SOLQ,SOL(NSOL+1),NSOLQ)
          NSOL = NSOL+NSOLQ
        ENDIF
        C(1) = 1.0
        C(2) = (P/2) + AA
        C(3) = K + BB
C SOLVE NEXT QUADRATIC.
        CALL QUADRATIC(C,NSOLQ,SOLQ)
        IF ( NSOLQ.GT.0 ) THEN
          CALL UCOPY(SOLQ,SOL(NSOL+1),NSOLQ)
          NSOL = NSOL+NSOLQ
        ENDIF
      ENDIF
C
  999 RETURN
      END
