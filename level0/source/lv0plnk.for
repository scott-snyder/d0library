      SUBROUTINE LV0PLNK
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Creates a permanent link area for the
C-                         Level 0 Hit bank links
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  13-JUL-1992   Freedy Nang
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:LV0LNK.INC'
      INTEGER ICALL
      DATA ICALL/0/
C----------------------------------------------------------------------
      IF (ICALL.LE.0) THEN
        ICALL=1
        CALL MZLINK(IXCOM,'/LV0LNK/',LV0LNK,LPLV0,LV0LNK)
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
