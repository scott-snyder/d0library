      SUBROUTINE L15_FRAMEWORK_INIT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : The initialization routine for the Level 1.5
C-     subsystem of L1SIM. Clears the L15_FW_AND_MISC common block and extracts
C-     parameters from the L1SIM_RCP RCP bank.
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
      INCLUDE 'D0$PARAMS:L1_FRAMEWORK.PARAMS'
      INCLUDE 'D0$PARAMS:L15_FRAMEWORK.PARAMS'
      INCLUDE 'D0$INC:L15_FRAMEWORK.INC'
      INCLUDE 'D0$PARAMS:L1SIM_CONTROL.PARAMS'
      INCLUDE 'D0$INC:L1SIM_CONTROL.INC'
C
      INTEGER TERM, TRIGGER
      CHARACTER*72 STRING
      INTEGER      IER
C
C
      DO TERM = L15_TERM_NUM_MIN, L15_TERM_NUM_MAX
        L15_TERM_STATE(TERM) = .FALSE.
        L15_TERM_ASSIGNED(TERM) = .FALSE.
      END DO
C
      DO TRIGGER = TRG_NUM_MIN, TRG_NUM_MAX
        ST_IS_L15(TRIGGER) = .FALSE.
        L15_TRIGGER_FIRED(TRIGGER) = .FALSE.
        DO TERM = L15_TERM_NUM_MIN, L15_TERM_NUM_MAX
          L15_TERM_ALLOCATE(TERM, TRIGGER) = .FALSE.
        END DO
C
        COUNT_ST_L15_POTENTIAL(TRIGGER) = 0
        COUNT_ST_L15_SKIPPED(TRIGGER) = 0
        COUNT_ST_L15_SUBMITTED(TRIGGER) = 0
        COUNT_ST_L15_PASSED(TRIGGER) = 0
        COUNT_ST_L15_REJECT(TRIGGER) = 0
      END DO
C
      COUNT_EVENT_L15_POTENTIAL = 0
      COUNT_EVENT_L15_SKIPPED = 0
      COUNT_EVENT_L15_SUBMITTED = 0
      COUNT_EVENT_L15_PASSED = 0
      COUNT_EVENT_L15_REJECT = 0
C
C       Pick up RCP parameters
      CALL L1UTIL_PICK_L1SIM_RCP
C       
C       Always calculate a complete Level 1.5 decision
      CALL EZGET(L15_CERTIFIED_RCPKEY, L15_CERTIFIED, IER)
      IF (IER .NE. 0) THEN
        CALL EZGET_ERROR_TEXT(IER,STRING)
        CALL ERRMSG (L15_CERTIFIED_RCPKEY, 'L15_FRAMEWORK_INIT',
     &    STRING,'F')
        GOTO 2000
      ENDIF
C
 2000 CONTINUE
      CALL EZRSET
C
C----------------------------------------------------------------------
  999 RETURN
      END
