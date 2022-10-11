      SUBROUTINE L1DBB_L15_SECTION(UPDATE_L15_SECTION)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fill the Level 1.5 Section of the Data Block. Also
C-     fill other Level 1.5 scalers in other portions of the Data Block.
C-
C-   Inputs  : UPDATE_L15_SECTION       Whether to update the Level 1.5 section
C-                                        of the Data Block
C-   Outputs : common block outputs
C-   Controls: none
C-
C-   Created  12-AUG-1992   Philippe Laurens, Steven Klocek
C-   Updated  29-JUN-1994   sFahey  added L15 data block section 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$PARAMS:L1_CALTRIG.PARAMS'
      INCLUDE 'D0$PARAMS:L1_FRAMEWORK.PARAMS'
      INCLUDE 'D0$PARAMS:L15_FRAMEWORK.PARAMS'
      INCLUDE 'D0$INC:L1FW_ANDOR_AND_MISC.INC'
      INCLUDE 'D0$INC:L1C_GLOBAL_RESULTS.INC'
      INCLUDE 'D0$PARAMS:L1DBB_DATA_BLOCK.PARAMS'
      INCLUDE 'D0$INC:L1DBB_DATA_BLOCK.INC'
      INCLUDE 'D0$INC:L1_SPECIFIC_TRIGGER.INC'
      INCLUDE 'D0$INC:L1C_EVENT.INC'
      INCLUDE 'D0$INC:L15_FRAMEWORK.INC'
      INCLUDE 'D0$PARAMS:L15CALDBB_DATA_BLOCK.PARAMS'
      INCLUDE 'D0$INC:L15CALDBB_DATA_BLOCK.INC'
C
      LOGICAL UPDATE_L15_SECTION
      INTEGER MASK, SPEC_TRIG, TERM, SCALER(2)
      INTEGER USED_BIT
      PARAMETER (USED_BIT = 3)
      INTEGER L15_ST_BITS_L
      PARAMETER (L15_ST_BITS_L = (L15_SPEC_TRIG_NUM_MAX
     &                            -L15_SPEC_TRIG_NUM_MIN+1)
     &                           /BYTE_LENGTH)
C
C       Level 1.5 Information
C       =====================
C
C       These three items always must be updated
C
      CALL L1UTIL_FIRST_BYTE_CODING (SCALER_L-1, LEVEL15_CYCLE_SCALER,
     &  COUNT_EVENT_L15_SUBMITTED)
      CALL L1UTIL_FIRST_BYTE_CODING (SCALER_L-1, 
     &  LEVEL15_POTENTIAL_SCALER, COUNT_EVENT_L15_POTENTIAL)
      CALL L1UTIL_FIRST_BYTE_CODING (SCALER_L-1, LEVEL15_SKIP_SCALER,
     &  COUNT_EVENT_L15_SKIPPED)
C
C       Make an additional copy of the Level 1.5 cycle and potential scalers. 
C
      CALL PRTRGR_FIRST_BYTE_DECODING(SCALER_L, LEVEL15_CYCLE_SCALER,
     &  LVL1_DATA_BLOCK, SCALER)
      CALL L1UTIL_FIRST_BYTE_CODING  (SCALER_L,
     &  ADDITIONAL_RESERVED_SCALERS, SCALER(1))
C
      CALL PRTRGR_FIRST_BYTE_DECODING(SCALER_L, 
     &  LEVEL15_POTENTIAL_SCALER, LVL1_DATA_BLOCK, SCALER)
      CALL L1UTIL_FIRST_BYTE_CODING  (SCALER_L,
     &  ADDITIONAL_RESERVED_SCALERS+SCALER_L, SCALER(1))
C
C       Did the event use Level 1.5
C       
      IF (UPDATE_L15_SECTION .EQV. .TRUE.) THEN
        CALL L1UTIL_FIRST_BYTE_CODING( 1, L1_5_RESERVED, 
     &                                 IBSET(0, USED_BIT))
      ELSE
        CALL L1UTIL_FIRST_BYTE_CODING( 1, L1_5_RESERVED, 0)
      ENDIF
C
C       The Level 1.5 section is only updated if a Level 1.5 cycle was
C       performed.
C
      IF (UPDATE_L15_SECTION .EQV. .FALSE.) GOTO 999
C
      CALL L1UTIL_FIRST_BYTE_CODING (SCALER_L-1,
     &  LEVEL15_PASS_SCALER, COUNT_EVENT_L15_PASSED)
      CALL L1UTIL_FIRST_BYTE_CODING (SCALER_L-1,
     &  LEVEL15_PASS_SCALER+4*SCALER_L, COUNT_EVENT_L15_PASSED 
     &                                  - INCREMENT_EVENT_L15_PASSED)
      CALL L1UTIL_FIRST_BYTE_CODING (SCALER_L-1,
     &  LEVEL15_FAIL_SCALER, COUNT_EVENT_L15_REJECT)
      CALL L1UTIL_FIRST_BYTE_CODING (SCALER_L-1,
     &  LEVEL15_FAIL_SCALER+4*SCALER_L, COUNT_EVENT_L15_REJECT 
     &                                  - INCREMENT_EVENT_L15_REJECT)
C
      CALL L1UTIL_FIRST_BYTE_CODING (L15_ST_BITS_L, L15_STATUS, 
     &                               FIRED_MASK)
C
      MASK = 0
      DO SPEC_TRIG = TRG_NUM_MIN, TRG_NUM_MAX
        IF ((ST_IS_L15(SPEC_TRIG) .EQV. .TRUE.)
     &      .AND. (L15_TRIGGER_FIRED(SPEC_TRIG) .EQV. .TRUE.)) THEN
          MASK = IBSET(MASK, SPEC_TRIG)
        ENDIF
      END DO
      CALL L1UTIL_FIRST_BYTE_CODING(L15_ST_BITS_L, 
     &                              L15_STATUS+2*L15_ST_BITS_L, MASK)
C
      MASK = 0
      DO SPEC_TRIG = TRG_NUM_MIN, TRG_NUM_MAX
        IF ((ST_IS_L15(SPEC_TRIG) .EQV. .TRUE.)
     &    .AND. (L15_TRIGGER_FIRED(SPEC_TRIG) .EQV. .FALSE.)
     &    .AND. (L15_ST_FIRED_AT_L1(SPEC_TRIG) .EQV. .TRUE.)) THEN
          MASK = IBSET(MASK, SPEC_TRIG)
        ENDIF
      END DO
      CALL L1UTIL_FIRST_BYTE_CODING(L15_ST_BITS_L,
     &                              L15_STATUS+3*L15_ST_BITS_L, MASK)
C
      MASK = 0
      DO TERM = L15_TERM_NUM_MIN, L15_TERM_NUM_MAX
        IF ((L15_TERM_ASSIGNED(TERM) .EQV. .TRUE.) 
     &    .AND. (L15_TERM_STATE(TERM) .EQV. .TRUE.)) THEN
          MASK = IBSET(MASK, TERM)
        ENDIF
      END DO
      CALL L1UTIL_FIRST_BYTE_CODING(4,
     &                              L15_STATUS+4*L15_ST_BITS_L, MASK)
C
      MASK = 0
      DO TERM = L15_TERM_NUM_MIN, L15_TERM_NUM_MAX
        IF (L15_TERM_ASSIGNED(TERM) .EQV. .TRUE.) THEN
          MASK = IBSET(MASK, TERM)
        ENDIF
      END DO
      CALL L1UTIL_FIRST_BYTE_CODING(4,
     &  L15_STATUS+6*L15_ST_BITS_L, MASK)
C
C   L15 data block section...
C
      L15CAL_CRATE_HEADER(6) = 0      ! NOT FULLY FILLED YET
C
C----------------------------------------------------------------------
  999 RETURN
      END
