      FUNCTION DVMOD(A,N)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : to calculate double precision module 
C-                         of vector A for N first elements.
C-
C-   Returned value  :     DVMOD
C-   Inputs  :             A - vector
C-                         N - number of elements
C-   Outputs : 
C-   Controls: 
C-
C-   Created   24-APR-1991   V. Burtovoy
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      DOUBLE PRECISION A(*),FF
      DOUBLE PRECISION DVMOD
      INTEGER I,N
C----------------------------------------------------------------------
C
      FF=0.D00
      DO I=1,N
         FF=FF+A(I)*A(I)
      END DO
      DVMOD=DSQRT(FF)
      RETURN
      END
