      SUBROUTINE L15FW_CLEAR_TERMS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set all the Level 1.5 Terms to .FALSE.
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created   3-DEC-1991   Philippe Laurens, Steven Klocek
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$PARAMS:L1_CALTRIG.PARAMS'
      INCLUDE 'D0$PARAMS:L1_FRAMEWORK.PARAMS'
      INCLUDE 'D0$PARAMS:L15_FRAMEWORK.PARAMS'
      INCLUDE 'D0$INC:L15_FRAMEWORK.INC'
C
      INTEGER TERM
C
      DO TERM = L15_TERM_NUM_MIN, L15_TERM_NUM_MAX
        L15_TERM_STATE(TERM) = .FALSE.
        L15_TERM_ASSIGNED(TERM) = .FALSE.
      END DO
C
C----------------------------------------------------------------------
  999 RETURN
      END
