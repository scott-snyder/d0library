C DEC/CMS REPLACEMENT HISTORY, Element KFUND.FOR
C *2    23-JUL-1989 22:47:57 ABACHI "UPDATE FOR NEW BETA RELEASE"
C *1    10-JUL-1989 04:08:28 ABACHI "UPDATE"
C DEC/CMS REPLACEMENT HISTORY, Element KFUND.FOR
      SUBROUTINE KFUND
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
      CALL PPURGE(ERRHND)
      CALL PDI('LKAT"', 1, 1, DISPL//'.ILKA"', ERRHND)
      CALL PDI('FOVU"', 1, 1, DISPL//'.IM44"', ERRHND)
      CALL PDI('WIND"', 1, 1, DISPL//'.IM44"', ERRHND)
      CALL PDI('MX33"', 1, 1, DISPL//'.IVWP"', ERRHND)
C
      CALL PPURGE(ERRHND)
C
  999 RETURN
      END
