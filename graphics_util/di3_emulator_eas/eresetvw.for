      SUBROUTINE ERESETVW
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To be used by escape function 30021 for
C-                         resetting the viewport-window parameter
C-                         to its current ones.
C-
C-   Inputs  :
C-   Outputs : 
C-   Controls: 
C-
C-   Created  22-OCT-1991   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GRFPAR.INC'
      INTEGER I
C
      DO I=1,6
        UVIEW(I) = CVIEW(I)
        UWIND(I) = CWIND(I)
      ENDDO
C
C----------------------------------------------------------------------
  999 RETURN
      END
