      LOGICAL FUNCTION L1UTIL_TRGR_DISP_DEFDUMP ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Call the routine which interactively selects
C-      sections to include in the event dump. Inform this routine that
C-      simulation is not being performed.
C-
C-   Returned value  : Success value. Always .TRUE.
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created   4-FEB-1992   Philippe Laurens, Steven Klocek
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      L1UTIL_TRGR_DISP_DEFDUMP = .TRUE.
      CALL L1DMP_SELECT_SECTIONS(.FALSE.)
C----------------------------------------------------------------------
  999 RETURN
      END
