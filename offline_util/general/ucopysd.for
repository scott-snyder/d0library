      SUBROUTINE UCOPYSD(BUFS,BUFD,N)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Copies single precision to double precision
C-   array
C-
C-   Inputs  : BUFS = single precision array
C-             N = number of words to copy
C-   Outputs : BUFD = double precision array
C-   Controls: 
C-
C-   Created   3-APR-1993   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      DOUBLE PRECISION BUFD(*)
      REAL BUFS(*)
      INTEGER I,N
C----------------------------------------------------------------------
      DO 10 I = 1 ,N  
        BUFD(I) = BUFS(I)
   10 CONTINUE
  999 RETURN
      END
