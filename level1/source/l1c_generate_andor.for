      SUBROUTINE L1C_GENERATE_ANDOR(NUM_ANDOR_USED, 
     &  ANDOR_STATES, ANDOR_INDICES)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Create a list of Andor Terms from the results of the
C-      Level 1 Calorimeter Trigger simulation.
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
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$PARAMS:L1_CALTRIG.PARAMS'
      INCLUDE 'D0$PARAMS:L1_FRAMEWORK.PARAMS'
      INCLUDE 'D0$INC:L1C_GLOBAL_RESULTS.INC'
      INCLUDE 'D0$INC:L1FW_ANDOR_AND_MISC.INC'
C
      INTEGER NUM_ANDOR_USED
      LOGICAL ANDOR_STATES(1:NUM_ANDOR)
      INTEGER ANDOR_INDICES(1:NUM_ANDOR)
C
      INTEGER ANDOR
      INTEGER TYPE, CMP, REF, CARD
C
C       Initialize sources of Andor Terms
C
      NUM_ANDOR_USED = 0
C
      DO ANDOR = ANDOR_NUM_MIN, ANDOR_NUM_MAX
        IF (LV1_ANDOR_TERM_TYPE(ANDOR, AO_THRSH_TYPE_INDEX) 
     &      .EQ. AO_THRSH_MPT) THEN
          NUM_ANDOR_USED = NUM_ANDOR_USED + 1
          ANDOR_INDICES(NUM_ANDOR_USED) = ANDOR
          CMP = LV1_ANDOR_TERM_TYPE(ANDOR, AO_THRSH_SUB1_INDEX)
          IF (TOTAL_MPT_CMP_RSLT(CMP) .EQ. 1) THEN
            ANDOR_STATES(NUM_ANDOR_USED) = .TRUE.
          ELSE
            ANDOR_STATES(NUM_ANDOR_USED) = .FALSE.
          ENDIF
C
C       Do global sum comparisons
        ELSEIF (LV1_ANDOR_TERM_TYPE(ANDOR, AO_THRSH_TYPE_INDEX) 
     &    .EQ. AO_THRSH_GSUM) THEN
          TYPE = LV1_ANDOR_TERM_TYPE(ANDOR, AO_THRSH_SUB1_INDEX)
          CMP = 
     &      MOD(LV1_ANDOR_TERM_TYPE(ANDOR, AO_THRSH_SUB2_INDEX), 4) + 1
          CARD = LV1_ANDOR_TERM_TYPE(ANDOR, AO_THRSH_SUB2_INDEX)/4 + 1
          NUM_ANDOR_USED = NUM_ANDOR_USED +1
          ANDOR_INDICES(NUM_ANDOR_USED) = ANDOR
          IF (GLOBAL_ENERGY_CMP_RSLT(CMP, CARD, TYPE) .EQ. 1) THEN
            ANDOR_STATES(NUM_ANDOR_USED)  = .TRUE.
          ELSE
            ANDOR_STATES(NUM_ANDOR_USED) = .FALSE.
          ENDIF
C
C       Do hot tower count comparisons
        ELSEIF (LV1_ANDOR_TERM_TYPE(ANDOR, AO_THRSH_TYPE_INDEX)
     &    .EQ. AO_THRSH_CNT) THEN
          REF = LV1_ANDOR_TERM_TYPE(ANDOR, AO_THRSH_SUB1_INDEX)
          CMP = LV1_ANDOR_TERM_TYPE(ANDOR, AO_THRSH_SUB2_INDEX)
          NUM_ANDOR_USED = NUM_ANDOR_USED + 1
          ANDOR_INDICES(NUM_ANDOR_USED) = ANDOR
          IF (HOT_TOWER_CMP_RSLT(MOD(CMP,CMP_PER_CARD)+1,
     &      CMP/CMP_PER_CARD+1, REF) .EQ. 1) THEN
            ANDOR_STATES(NUM_ANDOR_USED) = .TRUE.
          ELSE
            ANDOR_STATES(NUM_ANDOR_USED) = .FALSE.
          ENDIF
        ELSE
C       Do nothing
        ENDIF
      END DO
C----------------------------------------------------------------------
  999 RETURN
      END
