      LOGICAL FUNCTION L1_LEVEL15_NEEDED()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : A function which determines if the current 
C-      event needs to be run through Level 1.5. A Level 1.5 evaluation
C-      can be forced for every event which has a Level 1.5 Trigger pass Level
C-      1 by setting the RCP parameter L15_CERTIFIED to .TRUE.
C-      
C-
C-   Returned Value : .TRUE. if Level 1.5 is needed
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  17-OCT-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C-                              dummy routine for now
C-   Updated   3-DEC-1991   Philippe Laurens, Steven Klocek   
C-                      Now looks at Level 1.5 configuration and Level 1
C-                      Trigger fired to return correct decision.
C-   Updated   3-JAN-1992   Philippe Laurens, Steven Klocek   
C-                      All Specific Trigger variables use [0,31] for indices
C-   Updated   8-MAR-1992   Philippe Laurens, Steven Klocek   
C-                      Replace global states of Specific Triggers after L1 
C-                      with explicit number of Specific Triggers that passed
C-                      Level 1 (2 flavors: pure L1, and L15).
C-                      Remove Obsolete L15_CONFIRMED variable
C-   Updated  17-NOV-1992   Philippe Laurens, Steven Klocek   
C-                      Moved calculation of COUNT_EVENT_L15_POTENTIAL and
C-                      COUNT_EVENT_L15_SKIPPED from L1_AND_L15_FW_ANAL to here.
C-                      Moved assignment of L15_CYCLE_PERFORMED to L1SIM_EVENT.
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
      INCLUDE 'D0$INC:L1SIM_CONTROL.INC'
C
      INTEGER TRIGGER
C
      NUM_PURE_L1_SPTRG_PASS_L1 = 0
      NUM_L15_SPTRG_PASS_L1     = 0
      NUM_L15_SPTRG_SENT_L15    = 0
      NUM_L15_SPTRG_PASS_L15    = 0 
      L1_LEVEL15_NEEDED         = .FALSE.
      IF ( NUM_SPTRG_PASS_L1 .EQ. 0 ) GOTO 999
C
C       Find all Level 1.5 Triggers which have fired at Level 1. Record this
C       information for later analysis.
C       At the same time, find if any pure Level 1 Triggers have fired, and if
C       ANY Level 1.5 Triggers have fired at Level 1.
C
      DO TRIGGER = TRG_NUM_MIN, TRG_NUM_MAX
C
        L15_ST_FIRED_AT_L1(TRIGGER) = .FALSE.
        IF ( FIRED_TRIGGER(TRIGGER) .EQV. .TRUE. ) THEN
C
          IF ( ST_IS_L15(TRIGGER) .EQV. .TRUE. ) THEN
C
            L15_ST_FIRED_AT_L1(TRIGGER) = .TRUE.
            NUM_L15_SPTRG_PASS_L1 = NUM_L15_SPTRG_PASS_L1 + 1 
          ELSE
C
            NUM_PURE_L1_SPTRG_PASS_L1 = NUM_PURE_L1_SPTRG_PASS_L1 + 1 
          ENDIF
        ENDIF
C
      ENDDO
C
C       When the L1SIM.RCP flag L15_CERTIFIED is .TRUE., the Level 1.5 decesion
C       may not be skipped. (this is not the normal hardware operation)
C
      IF (L15_CERTIFIED .EQV. .TRUE.) THEN
C
        IF ( NUM_L15_SPTRG_PASS_L1 .GT. 0 ) THEN
          L1_LEVEL15_NEEDED = .TRUE.
          NUM_L15_SPTRG_SENT_L15 = NUM_L15_SPTRG_PASS_L1 
          COUNT_EVENT_L15_POTENTIAL = COUNT_EVENT_L15_POTENTIAL + 1
        ENDIF
C
C       When L15_CERTIFIED is .FALSE., Pure Level 1 Trigger firings
C       cause the L1.5 decesion to be skipped and Level 1.5 triggers that
C       passed their L1 requirements are passed on.
C
      ELSE
C
        IF ( NUM_L15_SPTRG_PASS_L1 .GT. 0 ) THEN
          COUNT_EVENT_L15_POTENTIAL = COUNT_EVENT_L15_POTENTIAL + 1
C
          IF ( NUM_PURE_L1_SPTRG_PASS_L1 .EQ. 0 ) THEN
            L1_LEVEL15_NEEDED = .TRUE.
            NUM_L15_SPTRG_SENT_L15 = NUM_L15_SPTRG_PASS_L1 
C
          ELSE
            COUNT_EVENT_L15_SKIPPED = COUNT_EVENT_L15_SKIPPED + 1
          ENDIF
        ENDIF
C
      ENDIF
C
C----------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
