      SUBROUTINE FIND_VEC_DIFF(P1,P2,NDIM,DIFF)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : FIND difference between two  vectors P1 AND P2
C-
C-   Inputs  : p1,P2 two vectors
C-             NDIM their dimension
C-   Outputs : DIFF magnitude of the difference squared.
C-   Controls: 
C-
C-   Created   3-JAN-1991   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL    P1(*),P2(*),DIFF
      INTEGER NDIM,I
C----------------------------------------------------------------------
      DIFF = 0.
      DO I = 1 ,NDIM 
        DIFF = DIFF +(P1(I)-P2(I))**2
      ENDDO
      DIFF = SQRT(DIFF)
  999 RETURN
      END
