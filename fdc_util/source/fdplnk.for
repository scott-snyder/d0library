      SUBROUTINE FDPLNK
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create a permanent link area for the FDC Hit
C-                         bank links
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  19-MAY-1989   Jeffrey Bantly
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:FDCLNK.INC'
      INTEGER ICALL
      DATA ICALL/0/
C----------------------------------------------------------------------
      IF(ICALL .LE. 0) THEN
        ICALL = 1
        CALL MZLINK(IXCOM,'/FDCLNK/',FDCLNK,LCDD3,FDCLNK)
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
