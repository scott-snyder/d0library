      SUBROUTINE ESTOREVW(N)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : To be used by escape function 30019 for
C-                         saving a particular viewport-wind parameter
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
      IF(N .GT. NP) THEN
        WRITE(6,1) NP
    1   FORMAT(' Number of saved viewport_window parameters exceed ',I3)
        GOTO 999
      ENDIF
C
      DO I=1,6
        SVIEW(N,I) = UVIEW(I)
        SWIND(N,I) = UWIND(I)
      ENDDO
C
C----------------------------------------------------------------------
  999 RETURN
      END
