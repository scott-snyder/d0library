      SUBROUTINE L1FW_CLEAR_ANDOR
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set the state of all of the Andor Terms to .FALSE.
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created   5-SEP-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$PARAMS:L1_CALTRIG.PARAMS'
      INCLUDE 'D0$PARAMS:L1_FRAMEWORK.PARAMS'
      INCLUDE 'D0$INC:L1FW_ANDOR_AND_MISC.INC'
C
      INTEGER ANDOR
C
      DO ANDOR = ANDOR_NUM_MIN, ANDOR_NUM_MAX
        ANDOR_TERM(ANDOR) = .FALSE.
        ANDOR_TERM_ASSIGNED(ANDOR) = .FALSE.
      END DO
C
C----------------------------------------------------------------------
  999 RETURN
      END
