      SUBROUTINE UCOPYDS(BUFD,BUFS,N)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Copies double precision to single precision
C-   array
C-
C-   Inputs  : BUFD = single precision array
C-             N = number of words to copy
C-   Outputs : BUFS = double precision array
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
        BUFS(I) = BUFD(I)
   10 CONTINUE
  999 RETURN
      END
