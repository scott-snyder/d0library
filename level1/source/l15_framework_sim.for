      SUBROUTINE L15_FRAMEWORK_SIM
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculate the states of the Level 1.5 Triggers.
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created   6-SEP-1991   Philippe Laurens, Steven Klocek   
C-                              dummy routine for now
C-   Updated   3-DEC-1991   Philippe Laurens, Steven Klocek   
C-                      Now computes correct Level 1.5 Trigger firing masks
C-                      from Level 1.5 configuration and Level 1.5 input terms.
C-   Updated  13-DEC-1991   Philippe Laurens, Steven Klocek  
C-                      Computes the AND of the terms rather than the OR. 
C-   Updated   3-JAN-1992   Philippe Laurens, Steven Klocek  
C-                      All Specific Trigger variables now use [0,31]
C-                        numbering 
C-   Updated  17-NOV-1992   Philippe Laurens, Steven Klocek  
C-                      Moved calculation of Level 1.5 Scalers here from
C-                      L1_AND_L15_FW_ANAL. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$PARAMS:L1_CALTRIG.PARAMS'
      INCLUDE 'D0$PARAMS:L1_FRAMEWORK.PARAMS'
      INCLUDE 'D0$PARAMS:L15_FRAMEWORK.PARAMS'
      INCLUDE 'D0$INC:L1_SPECIFIC_TRIGGER.INC'
      INCLUDE 'D0$INC:L15_FRAMEWORK.INC'
C
      INTEGER TRIGGER
      INTEGER TERM
      LOGICAL TRIGGER_HAS_TERM
      CHARACTER*80 MESSAGE
C
C       Check that all allocated terms have been given a state
C
      DO TRIGGER = TRG_NUM_MIN, TRG_NUM_MAX
        DO TERM = L15_TERM_NUM_MIN, L15_TERM_NUM_MAX
          IF (L15_TERM_ALLOCATE(TERM, TRIGGER) .EQV. .TRUE.) THEN
            IF (L15_TERM_ASSIGNED(TERM) .NEQV. .TRUE.) THEN
              CALL ERRMSG('SP TRIG USES UNKNOWN L15 STATE', 
     &          'L15_FRAMEWORK_SIM', 
     &          'LEVEL 1.5 Trigger uses an input term which ' // 
     &          'was never given a state for this event', 'W')
              WRITE (MESSAGE,50) TRIGGER, TERM
              CALL ERRMSG('SP TRIG USES UNKNOWN L15 STA ID',
     &          'L15_FRAMEWORK_SIM', MESSAGE, 'F')
   50         FORMAT( 'Specific Trigger: ',I4,' Term: ', I4 )
              GOTO 999
            ENDIF
          ENDIF
        END DO
      END DO
C
C       Calculate the L15 Trigger Firings from the input terms
C
      DO TRIGGER = TRG_NUM_MIN, TRG_NUM_MAX
C
C       Assume this trigger will not fire in Level 1.5
        L15_TRIGGER_FIRED(TRIGGER) = .FALSE.
C
C       If this is not a Level 1.5 Specific Trigger, move on to the next ST
        IF (ST_IS_L15(TRIGGER) .EQV. .FALSE.) GOTO 100
C
C       If the corresponding Level 1 Specific Trigger did not fire,
C         move on to the next trigger
        IF (FIRED_TRIGGER(TRIGGER) .EQV. .FALSE.) GOTO 100
C
C       Check the component terms of this trigger and find the logical AND
        L15_TRIGGER_FIRED(TRIGGER) = .TRUE.
        TRIGGER_HAS_TERM = .FALSE.
        DO TERM = L15_TERM_NUM_MIN, L15_TERM_NUM_MAX
          IF (L15_TERM_ALLOCATE(TERM, TRIGGER) .EQV. .TRUE.) THEN
            TRIGGER_HAS_TERM = .TRUE.
            IF (L15_TERM_STATE(TERM) .EQV. .FALSE.) THEN
              L15_TRIGGER_FIRED(TRIGGER) = .FALSE.
            ENDIF
          ENDIF
        END DO
C
C       The trigger should not fire if there are no terms assigned to it.
        IF (TRIGGER_HAS_TERM .EQV. .FALSE.) THEN
          L15_TRIGGER_FIRED(TRIGGER) = .FALSE.
        ENDIF
C
C       Now we can see if the Specific Trigger has passed L1.5
        IF ( L15_TRIGGER_FIRED(TRIGGER) .EQV. .TRUE. ) THEN
          NUM_L15_SPTRG_PASS_L15 = NUM_L15_SPTRG_PASS_L15 + 1
        ELSE 
          FIRED_TRIGGER(TRIGGER) = .FALSE. 
          FIRED_MASK = IBCLR(FIRED_MASK, TRIGGER)
        ENDIF
C
  100   CONTINUE
      END DO
C
C       Find the values of the Level 1.5 Scalers
C
      INCREMENT_EVENT_L15_PASSED = 0
      INCREMENT_EVENT_L15_REJECT = 0
      IF (    ( NUM_L15_SPTRG_PASS_L1 .NE. 0 ) 
     &  .AND. ( NUM_L15_SPTRG_SENT_L15 .NE. 0 ) ) THEN
C
        COUNT_EVENT_L15_SUBMITTED = COUNT_EVENT_L15_SUBMITTED + 1
C
        IF ( NUM_L15_SPTRG_PASS_L15 .NE. 0 ) THEN
          COUNT_EVENT_L15_PASSED = COUNT_EVENT_L15_PASSED + 1
          INCREMENT_EVENT_L15_PASSED = 1
        ELSE
          COUNT_EVENT_L15_REJECT = COUNT_EVENT_L15_REJECT + 1
          INCREMENT_EVENT_L15_REJECT = 1
        END IF
C
      ENDIF
C
C----------------------------------------------------------------------
  999 RETURN
      END
