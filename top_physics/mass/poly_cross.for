      REAL FUNCTION POLY_CROSS(X,PAR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : CALCULATES CROSS SECTION AT X FROM POLYNOMIAL FIT 
C-   IN PAR. 1ST WORD OF PAR = NUMBER OF COEEFS FOLLOWING.
C-   PAR(1) = N
C-   PAR(2) = COEFF OF X**0 ETC.
C-   PAR(N+1) = COEFF OF X**N-1.
C-   FOR A CUBIC , N=4
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   4-APR-1993   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    X,PAR(*)
      INTEGER N,I
C----------------------------------------------------------------------
      N = PAR(1)
      POLY_CROSS = 0.
      DO I = 1 , N
        POLY_CROSS = POLY_CROSS + PAR(1+I)*(X**(I-1))
      ENDDO
  999 RETURN
      END
