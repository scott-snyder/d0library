      SUBROUTINE L1_FW_AND_CT_CLEAR()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Clear the common blocks used by the simulator.
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created   5-SEP-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C-   Updated   5-NOV-1991   Philippe Laurens, Steven Klocek   
C-                      Added code to clear ST_PRESCALER.
C-   Updated  26-NOV-1991   Philippe Laurens, Steven Klocek   
C-                      Added code to initialize the Reference Sets according
C-                      to the new definition.
C-   Updated   7-JAN-1992   Philippe Laurens, Steven Klocek  
C-                      Added code to initialize new variables added to data
C-                        block and framework. 
C-                      Added code to initialize 5 byte scalers.
C-   Updated  17-AUG-1992   Philippe Laurens, Steven Klocek  
C-                      Initialize new scalers needed for D0 Note 967 Rev B 
C-                      Initialize new variables needed for recording
C-                      programming from additional COOR messages.
C-   Updated   1-JUL-1993   Philippe Laurens - MSU L1 Trigger  
C-                      Set Default Large Tile Reference Set to one more than
C-                      the maximum energy in a Large Tile.
C-                      Extend initialization of ST_VS_RS
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$PARAMS:L1_CALTRIG.PARAMS'
      INCLUDE 'D0$PARAMS:L1_FRAMEWORK.PARAMS'
      INCLUDE 'D0$INC:L1FW_ANDOR_AND_MISC.INC'
      INCLUDE 'D0$INC:L1_SPECIFIC_TRIGGER.INC'
      INCLUDE 'D0$INC:L1C_REFSET_AND_COUNT_THRESHOLDS.INC'
      INCLUDE 'D0$INC:L1C_GLOBAL_ENERGY_THRESHOLDS.INC'
      INCLUDE 'D0$PARAMS:L1DBB_DATA_BLOCK.PARAMS'
      INCLUDE 'D0$INC:L1DBB_DATA_BLOCK.INC'
      INCLUDE 'D0$INC:L1C_EVENT.INC'
C
      INTEGER SPECTRIG, REF, GEO_SECT, ANDOR, CMP, I, TERM
      INTEGER SIGN_ETA, MAGN_ETA, PHI, THRESH, TOWER
C
      CALL L1C_FUTURE_USE_INIT
C 
C       Level 1 Trigger Data Block presetting
C 
      DO I = 1, DATA_BLOCK_MAX
        LVL1_DATA_BLOCK(I) = 0
      ENDDO
C
      DO I = 1, TRGR_HEADER_LENGTH
        L1_CRATE_HEADER(I) = 0
      END DO
C
      DO I = 1, TRGR_TRAILER_LENGTH
        L1_CRATE_TRAILER(I) = 0
      END DO
C
C
      DO TOWER = EM_TOWER, HD_TOWER
        DO PHI = PHI_MIN, PHI_MAX
          DO MAGN_ETA = ETA_MIN, ETA_MAX
            DO SIGN_ETA = POS_ETA, NEG_ETA
              FADC_BYTE(SIGN_ETA, MAGN_ETA, PHI, TOWER) = 0
            END DO
          END DO
        END DO
      END DO
C
      DO REF = RS_SET_MIN, RS_SET_MAX
        DO PHI = PHI_MIN, PHI_MAX
          DO MAGN_ETA = ETA_MIN, ETA_MAX
            DO SIGN_ETA = POS_ETA, NEG_ETA
              EMT_THRSHLD(SIGN_ETA, MAGN_ETA, PHI, 
     &          EM_ET_REF_MIN + REF) = 255
              HDT_VETO(SIGN_ETA, MAGN_ETA, PHI,
     &          EM_ET_REF_MIN + REF) = 0.
              TOT_THRSHLD(SIGN_ETA, MAGN_ETA, PHI,
     &          TOT_ET_REF_MIN + REF) = 255
            END DO
          END DO
        END DO
      END DO
C
      DO REF = LT_REF_MIN, LT_REF_MAX
        DO PHI = LT_PHI_MIN, LT_PHI_MAX
          DO MAGN_ETA = LT_ETA_MIN, LT_ETA_MAX
            DO SIGN_ETA = POS_ETA, NEG_ETA
              LT_THRSHLD( SIGN_ETA, MAGN_ETA, PHI, REF) 
     &          = 4 * 8 * 255 + 1
            END DO
          END DO
        END DO
      END DO
C
      DO REF = EM_ET_REF_MIN, TOT_ET_REF_MAX
        DO CMP = TOWER_CNT_THRSH_MIN, TOWER_CNT_THRSH_MAX
          HOT_COUNT_REF(CMP, REF) = 0
        END DO
        DO SPECTRIG = TRG_NUM_MIN, TRG_NUM_MAX
          ST_VS_RS(SPECTRIG, REF) = .FALSE.
        END DO
      END DO
C
      DO REF = LT_REF_MIN, LT_REF_MAX
        DO SPECTRIG = TRG_NUM_MIN, TRG_NUM_MAX
          ST_VS_RS(SPECTRIG, REF) = .FALSE.
        END DO
      END DO
C
      DO GEO_SECT = GEO_NUM_MIN, GEO_NUM_MAX
        GS_FRONT_END_BUSY(GEO_SECT) = .FALSE.
        GS_STARTDGT(GEO_SECT) = .FALSE.
        DO SPECTRIG = TRG_NUM_MIN, TRG_NUM_MAX
          ST_STARTDGT(GEO_SECT, SPECTRIG) = .FALSE.
          FEBUSY_GS_TO_ST(GEO_SECT, SPECTRIG) = .FALSE.
        END DO
      END DO
C
      DO SPECTRIG = TRG_NUM_MIN, TRG_NUM_MAX
        FIRED_TRIGGER(SPECTRIG) = .FALSE.
        ST_LEVEL1_STATE(SPECTRIG) = .FALSE.
        ENABLE_SCALERS(1, SPECTRIG) = 0
        ENABLE_SCALERS(2, SPECTRIG) = 0
        ENABLE_SCALERS_INCREMENTED(SPECTRIG) = 0
        FIRED_SCALERS(1, SPECTRIG) = 0
        FIRED_SCALERS(2, SPECTRIG) = 0
        FIRED_SCALERS_INCREMENTED(SPECTRIG) = 0
        ST_PRESCALER(SPECTRIG) = 1
        PROGRAMMED_TRIGGER(SPECTRIG) = .FALSE.
        FSTD_ENABLED(SPECTRIG) = .FALSE.
        FSTD_ANDOR_FIRED(SPECTRIG) = .FALSE.
        ST_FRONT_END_BUSY_DISABLE(SPECTRIG) = .FALSE.
        ST_L2_DISABLE(SPECTRIG) = .FALSE.
        DO ANDOR = ANDOR_NUM_MIN, ANDOR_NUM_MAX
          SPECTRIG_ANDOR_ALLOC(ANDOR, SPECTRIG) = .FALSE.
          SPECTRIG_ANDOR_POLARITY(ANDOR, SPECTRIG) = .FALSE.
        END DO
        OBEY_FEBUSY(SPECTRIG) = .FALSE.
        OBEY_L2BUSY(SPECTRIG) = .FALSE.
        ST_ENABLED(SPECTRIG) = .TRUE.
        ST_FSTD_VETO(SPECTRIG) = .FALSE.
      END DO
C
      TRIGGER_SCALER(1) = 0
      TRIGGER_SCALER(2) = 0
      TRIGGER_SCALER_INCREMENTED = 0
      BEAM_SCALER(1) = 0
      BEAM_SCALER(2) = 0
      BEAM_SCALER_INCREMENTED = 0
      L0_FASTZ_GOOD_SCALER(1) = 0
      L0_FASTZ_GOOD_SCALER(2) = 0
      L0_FASTZ_GOOD_INCREMENTED = 0
C
      DO THRESH = GL_EMET_THRTYP, GL_TOTL2_THRTYP
        DO CMP = SUM_MIN, SUM_MAX
          GLOBAL_ENERGY_REF(MOD(CMP, 4) +1,CMP / 4 + 1, THRESH)
     &      = 0
        END DO
      END DO
C
      DO CMP = 1, MPT_CMP_MAX
        TOTAL_MPT_REF(CMP) = 0.
      END DO
C
      DO CMP = AO_THRSH_TYPE_INDEX, AO_THRSH_SUB2_INDEX
        DO ANDOR = ANDOR_NUM_MIN, ANDOR_NUM_MAX
          LV1_ANDOR_TERM_TYPE(ANDOR, CMP) = 0
        END DO
      END DO
C----------------------------------------------------------------------
  999 RETURN
      END
