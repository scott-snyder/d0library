      LOGICAL FUNCTION DLIMIT_FIN()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : STORE AWAY HISTS
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  23-FEB-1995   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      CALL HRPUT(0,'DLIMIT_HBOOK.DAT','NT')
  999 RETURN
      END
