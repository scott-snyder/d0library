      SUBROUTINE L2_HIDOPT(ID,OPT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Changes properties of histogram.
C-                         At present only option available is log scale.
C-
C-   Inputs  : ID - histogram ID number
C-             OPT - LOGY = store as log plot
C-   Outputs : none
C-   Controls: none
C-
C-   Created  19-FEB-1990   Jan Guida, Srini Rajagopalan
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBHST.INC'
      INCLUDE 'D0$INC:L2CLBH.INC'
      CHARACTER*(*) OPT
      CHARACTER*40 MSG
      INTEGER ID
C----------------------------------------------------------------------
C
      IF(OPT.EQ.'LOGY') THEN
        KH(2*LCHST(ID)+2) = 1
      ELSE
        WRITE(MSG,10)OPT
   10   FORMAT(' Illegal option ',A)
        CALL L2_HBERR('L2HIDOPT',MSG,ID)
      ENDIF
C
  999 RETURN
      END
