      FUNCTION LSQ_FIN()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Finish up LSQ
C-
C-   Returned value  : True if OK
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  20-DEC-1990   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL LSQ_FIN
      INTEGER IER
      INCLUDE 'D0$INC:LSQ_PARS.INC'
C----------------------------------------------------------------------
      LSQ_FIN = .TRUE.
      CALL LSQ_RZ_SAVE(' ',SUB_DIRECTORY,'ALL',IER)
      CALL RZEND(TOP_DIRECTORY)
  999 RETURN
      END
