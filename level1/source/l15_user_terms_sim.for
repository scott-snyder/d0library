      SUBROUTINE L15_USER_TERMS_SIM( NUM_L15_TERM_USED, L15_TERM_STATES,
     &  L15_TERM_INDICES)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Dummy routine, to be replaced with the simulation of
C-   the Level 1.5 Trigger subsystem.
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created   6-SEP-1991   Philippe Laurens, Steven Klocek
C-                              dummy routine for now
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NUM_L15_TERM_USED
      LOGICAL L15_TERM_STATES(32)
      INTEGER L15_TERM_INDICES(32)
C
      NUM_L15_TERM_USED = 0
C----------------------------------------------------------------------
  999 RETURN
      END
