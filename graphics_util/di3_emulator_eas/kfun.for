C DEC/CMS REPLACEMENT HISTORY, Element KFUN.FOR
C *2    23-JUL-1989 22:47:35 ABACHI "UPDATE FOR NEW BETA RELEASE"
C *1    10-JUL-1989 04:07:40 ABACHI "UPDATE"
C DEC/CMS REPLACEMENT HISTORY, Element KFUN.FOR
      SUBROUTINE KFUN
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  26-JUN-1989   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GRFPAR.INC/LIST'
      EXTERNAL ERRHND
C
      CALL PFN('LKAT"', 'LOOKAT', ERRHND)
      CALL PFN('FOVU"', 'FOV', ERRHND)
      CALL PFN('WIND"', 'WINDOW', ERRHND)
      CALL PFN('MX33"', 'MATRIX3', ERRHND)
C
CC      CALL PPURGE(ERRHND)
C
  999 RETURN
      END
