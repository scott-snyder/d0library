      LOGICAL FUNCTION l2em_ntuple_fill_reset()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : call fill_reset subroutine from D0USER frame
C-                         for L2EM_NTUPLE package
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  13-NOV-1993   James T. McKinley
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      l2em_ntuple_fill_reset = .TRUE.
      call fill_reset()
  999 RETURN
      END
