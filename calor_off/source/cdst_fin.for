      LOGICAL FUNCTION CDST_FIN()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Called at end of cdst package
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  29-SEP-1992   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      CDST_FIN = .true.
      CALL C_WZ_SUMMARY
  999 RETURN
      END
