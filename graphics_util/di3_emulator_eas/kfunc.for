C DEC/CMS REPLACEMENT HISTORY, Element KFUNC.FOR
C *2    23-JUL-1989 22:47:46 ABACHI "UPDATE FOR NEW BETA RELEASE"
C *1    10-JUL-1989 04:08:02 ABACHI "UPDATE"
C DEC/CMS REPLACEMENT HISTORY, Element KFUNC.FOR
      SUBROUTINE KFUNC
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
      CALL PCONN('LKAT"', 1, 1, DISPL//'.ILKA"', ERRHND)
      CALL PCONN('FOVU"', 1, 1, DISPL//'.IM44"', ERRHND)
      CALL PCONN('WIND"', 1, 1, DISPL//'.IM44"', ERRHND)
      CALL PCONN('MX33"', 1, 1, DISPL//'.IVWP"', ERRHND)
C
CCC      CALL PPURGE(ERRHND)
C
  999 RETURN
      END
