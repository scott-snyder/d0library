      SUBROUTINE L1DBB_DATA_BLOCK_BUILDER
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Builds the Level 1 Trigger Data Block.
C-
C-   Inputs  : LEVEL1_CAL_TRIG_RESULTS, SPECIFIC_TRIGGER and
C-             TRG_SIMUL_EVENT commons.
C-
C-   Outputs : LEVEL1_TRIGGER_DATA_BLOCK common.
C-   Controls: None.
C-
C-   Created   9-MAR-1990   Sylvain Tisserant (MSU)
C-   Revised  18-JUN-1990
C-   Updated  14-AUG-1990   James T. Linnemann   
C-   Updated  Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C-                          - Many more items are now recorded into the 
C-                            datablock. 
C-                          - Changed all occurances of JET_LIST_BUILDER to
C-                            L1DBB_JET_LIST_BUILDER. 
C-                          - Changed name of routine from
C-                            LEVEL1_DATA_BLOCK_BUILDER to
C-                            L1DBB_DATA_BLOCK_BUILDER. 
C-                          - Changed all occurances of FIRST_BYTE_CODING to
C-                            L1UTIL_FIRST_BYTE_CODING. 
C-                          - Changed all occurances of WHERE_IN_DATA_BLOCK to
C-                            L1UTIL_WHERE_WORD. 
C-                          - Replaced D0$PARAMS:LEVEL1_CAL_TRIG.PARAMS with
C-                            D0$PARAMS:L1_CALTRIG.PARAMS 
C-                          - Replaced D0$PARAMS:LEVEL1_FRAMEWORK.PARAMS with
C-                            D0$PARAMS:L1_FRAMEWORK.PARAMS 
C-                          - Replaced D0$INC:LEVEL1_CAL_TRIG_RESULTS.INC with
C-                            D0$INC:L1C_GLOBAL_RESULTS.INC 
C-                          - Replaced D0$INC:LEVEL1_FRAMEWORK.INC with
C-                            D0$INC:L1FW_ANDOR_AND_MISC.INC 
C-                          - Replaced D0$INC:LEVEL1_TRIGGER_DATA_BLOCK.INC
C-                            with D0$INC:L1DBB_DATA_BLOCK.INC 
C-                          - Replaced D0$INC:SPECIFIC_TRIGGER.INC with
C-                            D0$INC:L1_SPECIFIC_TRIGGER.INC 
C-                          - Replaced D0$INC:TRG_SIMUL_EVENT.INC with
C-                            D0$INC:L1C_EVENT.INC 
C-                          - Replaced
C-                            D0$PARAMS:LEVEL1_TRIGGER_DATA_BLOCK.PARAMS with
C-                            D0$PARAMS:L1DBB_DATA_BLOCK.PARAMS 
C-   Updated   3-JAN-1992   Philippe Laurens, Steven Klocek  
C-                          All Specific Trigger variables now use [0,31]
C-                            numbering
C-                          Start Digitize signals are now computed in the
C-                            framework.        
C-                          Store Level 1 Spec Trig state in datablock, 
C-                            while still using the final decision for the
C-                            crate header.
C-   Updated  17-AUG-1992   Philippe Laurens, Steven Klocek  
C-                          Add a few new scalers to match D0 Note 967 Rev B
C-   Updated   3-JUN-1993   Philippe Laurens - MSU L1 Trigger  
C-                          remove call to L1DBB_JET_LIST_BUILDER. Moved to 
C-                          to L1_FW_AND_CT_FILL, and use >official< routine
C-                          L1UTIL_JET_LIST_BUILDER (same as used by Level 2)
C-
C----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$PARAMS:L1_CALTRIG.PARAMS'
      INCLUDE 'D0$PARAMS:L1_FRAMEWORK.PARAMS'
      INCLUDE 'D0$INC:L1FW_ANDOR_AND_MISC.INC'
      INCLUDE 'D0$INC:L1C_GLOBAL_RESULTS.INC'
      INCLUDE 'D0$PARAMS:L1DBB_DATA_BLOCK.PARAMS'
      INCLUDE 'D0$INC:L1DBB_DATA_BLOCK.INC'
      INCLUDE 'D0$INC:L1_SPECIFIC_TRIGGER.INC'
      INCLUDE 'D0$INC:L1C_EVENT.INC'
C
      INTEGER    PHI_HALF
      PARAMETER (PHI_HALF = (PHI_MAX - PHI_MIN + 1)/2)
C
      INTEGER  I1, I2, I, J, TRIGGER, ADDRESS, IW, BIT, REF
      INTEGER  CHANNEL, ETA_SIGN, ETA, PHI
      INTEGER  ANDOR, SPEC_TRIG, GEO_SECT
      INTEGER  L1FIRED_MASK
      INTEGER SCALER(2)
C
C----------------------------------------------------------------------
C
C     Transfers the previous event and reset current event
C     ====================================================
C
      I1 = I_CURRENT_EVT
      I2 = I_PREVIOUS_EVT
      DO I = 1, EVENT_MAX
        LVL1_DATA_BLOCK(I2) = LVL1_DATA_BLOCK(I1)
        LVL1_DATA_BLOCK(I1) = 0
        I1 = I1 + 1
        I2 = I2 + 1
      ENDDO
C
C     Fills Scalers, Specific Trigger FSTD and Specific Trigger Fired
C     ===============================================================
C
      FIRED_MASK = 0
      L1FIRED_MASK = 0
      DO TRIGGER = TRG_NUM_MIN, TRG_NUM_MAX 
C                                                       Specific Trigger Scalers
C                                                       ------------------------
C
        ADDRESS = SP_TRG_SCALERS + ( 2 * TRIGGER * SCALER_L)
        CALL L1UTIL_FIRST_BYTE_CODING (SCALER_L, ADDRESS,
     +                            FIRED_SCALERS(1,TRIGGER) )
        ADDRESS = ADDRESS + SCALER_L
        CALL L1UTIL_FIRST_BYTE_CODING (SCALER_L, ADDRESS,
     +                            ENABLE_SCALERS(1,TRIGGER) )
C
C                                                          Specific Trigger FSTD
C                                                          ---------------------
C       The following works because no vetoes are applied to Specific Triggers.
C       If a Spec Trig is not fired, then not all of its Andor Terms have
C       fired.
        ADDRESS = SP_TRG_FSTD + (TRIGGER/4)
        CALL L1UTIL_WHERE_WORD (ADDRESS, IW, BIT)
        BIT = BIT + MOD(TRIGGER,4)
        IF (FSTD_ENABLED(TRIGGER) .EQV. .TRUE.) THEN
          CALL SBIT1 (LVL1_DATA_BLOCK(IW), BIT)
        ENDIF
        IF(FSTD_ANDOR_FIRED(TRIGGER) .EQV. .TRUE.) THEN
          BIT = BIT + 4
          CALL SBIT1 (LVL1_DATA_BLOCK(IW), BIT)
        ENDIF
C                                                         Specific Trigger Fired
C                                                         ----------------------
        IF (FIRED_TRIGGER(TRIGGER) .EQV. .TRUE.) THEN
          CALL SBIT1 (FIRED_MASK, TRIGGER+1)
        ENDIF
        IF (ST_LEVEL1_STATE(TRIGGER) .EQV. .TRUE.) THEN
          CALL SBIT1 (L1FIRED_MASK, TRIGGER+1)
        ENDIF
      ENDDO
C
      CALL L1UTIL_FIRST_BYTE_CODING (SCALER_L, TRG_NUM_SCALER, 
     &  TRIGGER_SCALER)
      CALL L1UTIL_FIRST_BYTE_CODING (SCALER_L, DBLOCK_NUM_SCALER,
     &  TRIGGER_SCALER)
C
      CALL L1UTIL_FIRST_BYTE_CODING (SCALER_L, BEAM_CROSS_SCALER, 
     &  BEAM_SCALER)
      CALL L1UTIL_FIRST_BYTE_CODING (SCALER_L, FASTZ_GOOD_SCALER,
     &  L0_FASTZ_GOOD_SCALER)
      ADDRESS = SP_TRG_FIRED
      DO I = 1, 4
        CALL L1UTIL_FIRST_BYTE_CODING (4, ADDRESS, L1FIRED_MASK)
        ADDRESS = ADDRESS + 4
      ENDDO
C
C       Andor input terms
      DO ANDOR = ANDOR_NUM_MIN, ANDOR_NUM_MAX, BYTE_LENGTH
        BIT = 0
        DO I = 1, BYTE_LENGTH
          IF (ANDOR_TERM(ANDOR + I -1) .EQV. .TRUE.) THEN
            CALL SBIT1(BIT, I)
          ELSE
            CALL SBIT0(BIT, I)
          ENDIF
        END DO
        CALL L1UTIL_FIRST_BYTE_CODING(1, 
     &    ANDOR_01_TO_16 +ANDOR/BYTE_LENGTH, BIT)
        CALL L1UTIL_FIRST_BYTE_CODING(1, 
     &    ANDOR_17_TO_32 +ANDOR/BYTE_LENGTH, BIT)
      END DO
C
C       Start Digitize signals
      BIT = 0
C       Set all Start Digitize signals for each trigger
      DO GEO_SECT = GEO_NUM_MIN, GEO_NUM_MAX
        IF (GS_STARTDGT(GEO_SECT) .EQV. .TRUE.) THEN
          CALL SBIT1(BIT,GEO_SECT+1)
        ENDIF
      END DO
C
C       Clear Start Digitize Off signals
C       Digitize off does not apply to Datablock 15-OCT-1991 
C      DO GEO_SECT = GEO_NUM_MIN, GEO_NUM_MAX
C        IF (DGTZOFF(GEO_SECT) .EQV. .TRUE.) THEN
C          CALL SBIT0(BIT, GEO_SECT+1)
C        ENDIF
C      END DO
C
      CALL L1UTIL_FIRST_BYTE_CODING(4, START_DIGITIZE, BIT)
C
C     Global Quantities
C     =================
C
      CALL L1UTIL_FIRST_BYTE_CODING (GLOBAL_L, EM_ET_TOTAL,
     +                        GLOBAL_ENERGY(GL_EMET_THRTYP) )
      CALL L1UTIL_FIRST_BYTE_CODING (GLOBAL_L, HD_ET_TOTAL,
     +                        GLOBAL_ENERGY(GL_HDET_THRTYP) )
      CALL L1UTIL_FIRST_BYTE_CODING (GLOBAL_L, PX_TOTAL, TOTAL_PX)
      CALL L1UTIL_FIRST_BYTE_CODING (GLOBAL_L, PY_TOTAL, TOTAL_PY)
      CALL L1UTIL_FIRST_BYTE_CODING (GLOBAL_L, TOT_ET_TOTAL,
     +                        GLOBAL_ENERGY(GL_TOTET_THRTYP) )
      CALL L1UTIL_FIRST_BYTE_CODING (GLOBAL_L, MPT_TOTAL, TOTAL_MPT)
      CALL L1UTIL_FIRST_BYTE_CODING (GLOBAL_L, EM_L2_TOTAL,
     +                        GLOBAL_ENERGY(GL_EML2_THRTYP)  )
      CALL L1UTIL_FIRST_BYTE_CODING (GLOBAL_L, HD_L2_TOTAL,
     +                        GLOBAL_ENERGY(GL_HDL2_THRTYP)  )
      CALL L1UTIL_FIRST_BYTE_CODING (GLOBAL_L, TOT_L2_TOTAL,
     +                        GLOBAL_ENERGY(GL_TOTL2_THRTYP) )
      ADDRESS = HOT_TOWER_FINAL
      DO REF = EM_ET_REF_MIN, TOT_ET_REF_MAX
        CALL L1UTIL_FIRST_BYTE_CODING (2, ADDRESS, HOT_TOWER_COUNT(REF))
        ADDRESS = ADDRESS + 2
      ENDDO
C
C     Fast Vertex Information
C     =======================
C
      CALL L1UTIL_WHERE_WORD (FAST_VERTEX, IW, J)
      CALL SBYT (LEVEL_0_NZ, LVL1_DATA_BLOCK(IW), J, 5)
      IF(LEVEL_0_OK) CALL SBIT1(LVL1_DATA_BLOCK(IW), J+5)
C
C     Trigger Tower FADC values
C     =========================
C
      CALL L1UTIL_WHERE_WORD (TT_FADC, IW, J)
      DO CHANNEL = EM_TOWER, HD_TOWER
        DO ETA_SIGN = POS_ETA, NEG_ETA
          DO ETA = ETA_MIN, ETA_MAX
            DO PHI = PHI_MIN, PHI_MIN+PHI_HALF-1
              CALL SBYT (FADC_BYTE(ETA_SIGN,ETA,PHI,CHANNEL),
     +                   LVL1_DATA_BLOCK(IW), J, BYTE_LENGTH    )
              J = J + BYTE_LENGTH
              CALL SBYT (FADC_BYTE(ETA_SIGN,ETA,PHI+PHI_HALF,CHANNEL),
     +                   LVL1_DATA_BLOCK(IW), J, BYTE_LENGTH    )
              J = J + BYTE_LENGTH
              IF(J.GT.LONG_WORD_LENGTH) THEN
                IW = IW + 1
                J  = FIRST_BYTE
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDDO
C
C     Jet Lists
C     =========
C
      ADDRESS = JET_PROGRAMMING
      DO REF = EM_ET_REF_MIN, TOT_ET_REF_MAX
        CALL L1UTIL_FIRST_BYTE_CODING (4, ADDRESS, 
     &    PROGRAMMING_MASK(REF) )
        ADDRESS = ADDRESS + 4
      ENDDO
C
C     Same thing for Large Tile Jet Lists
      ADDRESS = LRG_TILE_JET_PROGRAMMING
      DO REF = LT_REF_MIN, LT_REF_MAX
        CALL L1UTIL_FIRST_BYTE_CODING (4, ADDRESS, 
     &    PROGRAMMING_MASK(REF) )
        ADDRESS = ADDRESS + 4
      ENDDO
C
C   removed  3-JUN-1993 , now use official l1util_jet_list_builder, 
C   but call from L1_FW_AND_CT_FILL, since the rest of the data block needs to
C   filled already
C      CALL L1DBB_JET_LIST_BUILDER
C
C       Make an additional copy of some of the scalers
C
      CALL PRTRGR_FIRST_BYTE_DECODING(SCALER_L, FASTZ_GOOD_SCALER,
     &  LVL1_DATA_BLOCK, SCALER)
      CALL L1UTIL_FIRST_BYTE_CODING  (SCALER_L, 
     &  ADDITIONAL_RESERVED_SCALERS+2*SCALER_L, SCALER)
C
      CALL PRTRGR_FIRST_BYTE_DECODING(SCALER_L, F2_SCALER,
     &  LVL1_DATA_BLOCK, SCALER)
      CALL L1UTIL_FIRST_BYTE_CODING  (SCALER_L,
     &  ADDITIONAL_RESERVED_SCALERS+3*SCALER_L, SCALER)
C
      RETURN
      END
