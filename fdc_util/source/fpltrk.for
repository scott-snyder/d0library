      SUBROUTINE FPLTRK
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create a permanent link area for the FDC Track
C-                         bank links
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  26-FEB-1990   Jeffrey Bantly   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:FDLTRK.INC'
      INTEGER ICALL
      DATA ICALL/0/
C----------------------------------------------------------------------
      IF(ICALL .LE. 0) THEN
        ICALL = 1
        CALL MZLINK(IXCOM,'/FDLTRK/',LFTRH,LFTSG(1,2),LFTRH)
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
