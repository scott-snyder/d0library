      SUBROUTINE ERECALVW(N)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To be used by escape function 30020 for
C-                         recalling a particular viewport-window parameter
C-                         and setting.
C-
C-   Inputs  :  N   Ilist(1) given by escape function
C-   Outputs : 
C-   Controls: 
C-
C-   Created  22-OCT-1991   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER N
      INCLUDE 'D0$INC:GRFPAR.INC'
      INTEGER I
C
      DO I=1,6
        CVIEW(I) = UVIEW(I)
        CWIND(I) = UWIND(I)
        UVIEW(I) = SVIEW(N,I)
        UWIND(I) = SWIND(N,I)
      ENDDO
C
C----------------------------------------------------------------------
  999 RETURN
      END
