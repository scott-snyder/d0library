      SUBROUTINE L1_CALTRIG_SIM(NUM_ANDOR_USED, ANDOR_STATES,
     &  ANDOR_INDICES)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Simulate the Calorimeter Trigger subsystem
C-
C-   Inputs  : none
C-   Outputs : NUM_ANDOR_USED   The number of Andor Terms this routine is
C-                              returning.
C-             ANDOR_STATES     The state of each returned Andor Term.
C-             ANDOR_INDICES    The corresponding Andor Term number for 
C-                              each returned Andor Term.
C-   Controls: none
C-
C-   Created   5-SEP-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$PARAMS:L1_CALTRIG.PARAMS'
      INCLUDE 'D0$PARAMS:L1_FRAMEWORK.PARAMS'
      INCLUDE 'D0$PARAMS:L1SIM_CONTROL.PARAMS'
      INCLUDE 'D0$INC:L1SIM_CONTROL.INC'
      INCLUDE 'D0$INC:L1SIM_CONTROL_STRINGS.INC'
C
      INTEGER NUM_ANDOR_USED
      LOGICAL ANDOR_STATES(1:NUM_ANDOR)
      INTEGER ANDOR_INDICES(1:NUM_ANDOR)
C
      LOGICAL FAST_OUT
C
C      VERIFY_FAST_SIMULATION is a flag which when set .TRUE. causes the
C      program to check the results of the fast simulator algorithm against 
C      the results of the normal simulator algorithm.
C
      IF (VERIFY_FAST_SIMULATION .EQV. .FALSE.) THEN
        IF (DO_FAST_CALTRIG .EQV. .TRUE.) THEN
          CALL L1C_FAST_CALTRIG 
        ELSE
          CALL L1C_SIMUL_CALTRIG
        ENDIF
      ELSE
        CALL L1C_FAST_CALTRIG
        CALL L1C_BACKUP_CALTRIG_RSLT
        CALL L1C_SIMUL_CALTRIG
      ENDIF
C
      CALL L1C_GENERATE_ANDOR(NUM_ANDOR_USED,
     &  ANDOR_STATES,ANDOR_INDICES)
C
C----------------------------------------------------------------------
  999 RETURN
C
C
C
C#######################################################################
      ENTRY L1C_USING_FAST_SIMULATION(FAST_OUT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return whether fast simulation is being used.
C-
C-   Inputs  : none
C-   Outputs : FAST_OUT   .TRUE. if fast simulation is being used.
C-   Controls: none
C-
C-   Created  17-OCT-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C----------------------------------------------------------------------
      FAST_OUT = DO_FAST_CALTRIG
      RETURN
C----------------------------------------------------------------------

      END
