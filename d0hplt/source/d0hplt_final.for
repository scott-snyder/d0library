      FUNCTION D0HPLT_FINAL()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : End DI3000.
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  21-FEB-1991   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL D0HPLT_FINAL
C----------------------------------------------------------------------
      D0HPLT_FINAL = .TRUE.
      CALL DI3_END
  999 RETURN
      END
