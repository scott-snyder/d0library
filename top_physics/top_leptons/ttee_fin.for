      FUNCTION TTEE_FIN()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Finish up analysis
C-
C-   Returned value  : True if OK
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   2-OCT-1990   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL TTEE_FIN
      INTEGER IER
C----------------------------------------------------------------------
C
      CALL NTUPLE_END
      TTEE_FIN = .TRUE.
C
  999 RETURN
      END
