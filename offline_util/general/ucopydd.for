      SUBROUTINE UCOPYDD(BUFD,BUFS,N)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Copies double precision to DOUBLE precision
C-   array
C-
C-   Inputs  : BUFD = double precision array
C-             N = number of words to copy
C-   Outputs : BUFS = OUTPUT DOUBLE  precision array
C-   Controls: 
C-
C-   Created   3-APR-1993   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      DOUBLE PRECISION BUFD(*)
      DOUBLE PRECISION BUFS(*)
      INTEGER I,N
C----------------------------------------------------------------------
      DO 10 I = 1 ,N  
        BUFS(I) = BUFD(I)
   10 CONTINUE
  999 RETURN
      END
