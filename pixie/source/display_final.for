      FUNCTION DISPLAY_FINAL ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  25-FEB-1991   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL DISPLAY_FINAL
C----------------------------------------------------------------------
      DISPLAY_FINAL = .TRUE.
      CALL DI3_END
  999 RETURN
      END
