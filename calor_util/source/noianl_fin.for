      LOGICAL FUNCTION NOIANL_FIN()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Finish up NOIANL package
C-
C-   Returned value  : True if OK
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  20-SEP-1991   Allen I. Mincer
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IER
C----------------------------------------------------------------------
      NOIANL_FIN = .TRUE.
C Dump statistics of pileup events
      CALL NOI_PILEUP_STAT(1)
C----------------------------------------------------------------------
  999 RETURN
      END
