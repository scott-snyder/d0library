      SUBROUTINE L2_HNOENT(ID,NENTR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return number of entried for selected ID
C-
C-   Inputs  : ID = Histogram ID number
C-   Outputs : NENTR = Number of entries.
C-   Controls: none
C-
C-   Created  22-JAN-1990   Jan Guida, Srini Rajagopalan
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBHST.INC'
      INCLUDE 'D0$INC:L2CLBH.INC'
C
      INTEGER ID,NENTR
      LOGICAL L2_HEXIST
C----------------------------------------------------------------------
C
C  *** Check if Histogram exists
C
      IF (.NOT.L2_HEXIST(ID)) THEN
        CALL L2_HBERR('L2HNOENT','Histogram not booked',ID)
        NENTR = 0
      ELSE
        NENTR = IH(LCHST(ID)+3)
      ENDIF
C
  999 RETURN
      END
