      SUBROUTINE L2_GENERIC_PARAMETERS(NEWPAR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : initialization of downloaded parameters
C-      for Missing ET level 2 filter
C-      Always forget previously stored parameters
C-
C-   Inputs  : NEWPAR : [BYTE] if it's equal to zero, ignore
C-      this run begin--nothing new downloaded.  It's the number of sets of
C-      parameters downloaded for THIS tool.
CC-   Outputs :
C-   Controls:
C-
C-   Created  29-JUN-1991   James T. Linnemann
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      BYTE NEWPAR
C----------------------------------------------------------------------
  999 RETURN
      END
