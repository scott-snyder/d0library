      SUBROUTINE L1UTIL_TRGR_UNPACK(LTRGR_LEVEL1, CURRENT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Unpack a TRGR bank into internal variables.
C-
C-   Inputs  : LTRGR_LEVEL1    The ZEBRA index of the desired TRGR bank.
C-             CURRENT  Indicates whether to unpack the current event data or
C-                      the previous event data.
C-   Outputs : none
C-   Controls: none
C-
C-   Created   6-JAN-1992   Philippe Laurens, Steven Klocek
C-   Updated  24-FEB-1992   Philippe Laurens, Steven Klocek  
C-                            change argument meaning from LTRGR to
C-                            LTRGR_LEVEL1, i.e. beginning of data. 
C-                      Now uses L1UTIL_ADC_COUNT_UNPACK to unpack ADC bytes. 
C-   Updated  16-NOV-1992   Philippe Laurens, Steven Klocek  
C-                      Global sums are all signed quantities, 
C-                      and Missing Pt is a one byte quantity.
C-
C----------------------------------------------------------------------
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
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER LTRGR_LEVEL1
      LOGICAL CURRENT
C
      INTEGER ADDRESS, COUNT, BIT, FIRST_BIT
      INTEGER TRIGGER, ANDOR, GEO_SECT, REF_SET
      INTEGER IW
      INTEGER MASK
      INTEGER FIVE_BYTE(2)
      INTEGER JBYT, JBIT
      EXTERNAL JBYT, JBIT
      INTEGER IOR, IEOR
      LOGICAL BTEST
C&IF VAXVMS
C&ELSE
C&      EXTERNAL BTEST, IOR, IEOR
C&ENDIF
      INTEGER CHANNEL, ETA_SIGN, ETA, PHI
C
      INTEGER    PHI_HALF
      PARAMETER (PHI_HALF = (PHI_MAX - PHI_MIN + 1)/2)
      INTEGER SIGN_BIT
      PARAMETER (SIGN_BIT = FOURTH_BYTE - 1)
      INTEGER L0_SIGN_BIT
      PARAMETER (L0_SIGN_BIT = 4)
      INTEGER L0_GOOD_BIT
      PARAMETER (L0_GOOD_BIT = 5)
      INTEGER TOWER_COUNT_L
      PARAMETER (TOWER_COUNT_L = 2)
C
C       Copy the Crate Header, Data Block, and Crate Trailer from TRGR bank.
C
      DO COUNT = 1, TRGR_HEADER_LENGTH
        L1_CRATE_HEADER(COUNT) = IQ( LTRGR_LEVEL1 + COUNT - 1 )
      ENDDO
C
      L1_WORD_COUNT = IQ(LTRGR_LEVEL1 + TRGR_HEADER_LENGTH )
C
      DO COUNT = 1, DATA_BLOCK_MAX
        LVL1_DATA_BLOCK(COUNT) 
     &    = IQ(LTRGR_LEVEL1 + TRGR_HEADER_LENGTH + COUNT)
      END DO
C
      DO COUNT = 1, TRGR_TRAILER_LENGTH
        L1_CRATE_TRAILER(COUNT) = 
     &    IQ(LTRGR_LEVEL1 + TRGR_HEADER_LENGTH + DATA_BLOCK_MAX + COUNT)
      END DO
C
C     Move the PREVIOUS event data if requested
C
      IF (CURRENT .EQV. .FALSE.) THEN
        DO COUNT = 1, (PREVIOUS_EVENT -1)/2 
          LVL1_DATA_BLOCK(COUNT) 
     &      = LVL1_DATA_BLOCK(COUNT + (PREVIOUS_EVENT - 1)/2)
        END DO
      ENDIF
C
C       Unpack the scalers
C
      DO TRIGGER = TRG_NUM_MIN, TRG_NUM_MAX 
C        
        ADDRESS = SP_TRG_SCALERS + ( 2 * TRIGGER * SCALER_L)
        CALL PRTRGR_FIRST_BYTE_DECODING(SCALER_L, ADDRESS, 
     &                                  LVL1_DATA_BLOCK,
     &                                  FIRED_SCALERS(1,TRIGGER))
        ADDRESS = ADDRESS + PREVIOUS_EVENT - BASE_ADDRESS
        CALL PRTRGR_FIRST_BYTE_DECODING(SCALER_L, ADDRESS, 
     &                                  LVL1_DATA_BLOCK, FIVE_BYTE)
        CALL L1UTIL_SCALER_SUBTRACT(FIRED_SCALERS(1,TRIGGER), 
     &    FIVE_BYTE, FIRED_SCALERS_INCREMENTED(TRIGGER))
C
        ADDRESS = ADDRESS - PREVIOUS_EVENT + BASE_ADDRESS + SCALER_L
        CALL PRTRGR_FIRST_BYTE_DECODING(SCALER_L, ADDRESS,
     &                                  LVL1_DATA_BLOCK,
     &                                  ENABLE_SCALERS(1, TRIGGER))
        ADDRESS = ADDRESS + PREVIOUS_EVENT - BASE_ADDRESS
        CALL PRTRGR_FIRST_BYTE_DECODING(SCALER_L, ADDRESS,
     &                                  LVL1_DATA_BLOCK, FIVE_BYTE)
        CALL L1UTIL_SCALER_SUBTRACT(ENABLE_SCALERS(1, TRIGGER),
     &    FIVE_BYTE, ENABLE_SCALERS_INCREMENTED(TRIGGER))
      END DO
C
      CALL PRTRGR_FIRST_BYTE_DECODING (SCALER_L, TRG_NUM_SCALER, 
     &                                  LVL1_DATA_BLOCK,
     &                                  TRIGGER_SCALER)
      CALL PRTRGR_FIRST_BYTE_DECODING (SCALER_L, 
     &          TRG_NUM_SCALER + PREVIOUS_EVENT - BASE_ADDRESS, 
     &                                 LVL1_DATA_BLOCK,
     &                                 FIVE_BYTE)
      CALL L1UTIL_SCALER_SUBTRACT(TRIGGER_SCALER, FIVE_BYTE,
     &  TRIGGER_SCALER_INCREMENTED)
C
      CALL PRTRGR_FIRST_BYTE_DECODING (SCALER_L, BEAM_CROSS_SCALER, 
     &                                  LVL1_DATA_BLOCK,
     &                                  BEAM_SCALER)
      CALL PRTRGR_FIRST_BYTE_DECODING (SCALER_L, 
     &          BEAM_CROSS_SCALER + PREVIOUS_EVENT - BASE_ADDRESS, 
     &                                  LVL1_DATA_BLOCK,
     &                                  FIVE_BYTE)
      CALL L1UTIL_SCALER_SUBTRACT(BEAM_SCALER, FIVE_BYTE,
     &  BEAM_SCALER_INCREMENTED)
C
C       Specific Trigger FSTD
C
      DO TRIGGER = TRG_NUM_MIN, TRG_NUM_MAX
        ADDRESS = SP_TRG_FSTD + (TRIGGER/4)
        CALL L1UTIL_WHERE_WORD (ADDRESS, IW, BIT)
        BIT = BIT + MOD(TRIGGER,4)
        IF (JBIT(LVL1_DATA_BLOCK(IW), BIT) .EQ. 1) THEN
          FSTD_ENABLED(TRIGGER) = .TRUE.
        ELSE
          FSTD_ENABLED(TRIGGER) = .FALSE.
        ENDIF
C
        BIT = BIT + 4
        IF (JBIT(LVL1_DATA_BLOCK(IW), BIT) .EQ. 1) THEN
          FSTD_ANDOR_FIRED(TRIGGER) = .TRUE.
        ELSE
          FSTD_ANDOR_FIRED(TRIGGER) = .FALSE.
        ENDIF
      END DO
C
C       Andor input terms
C
      DO ANDOR = ANDOR_NUM_MIN, ANDOR_NUM_MAX, BYTE_LENGTH
        ADDRESS = ANDOR_01_TO_16 + ANDOR / BYTE_LENGTH
        CALL L1UTIL_WHERE_WORD(ADDRESS, IW, FIRST_BIT)
        DO BIT = 0, BYTE_LENGTH-1
          ANDOR_TERM(ANDOR+BIT) 
     &      = BTEST(LVL1_DATA_BLOCK(IW), BIT+FIRST_BIT-1)
        END DO
      END DO
C
C       Front end busy
C
      CALL PRTRGR_FIRST_BYTE_DECODING(NUM_TOT_GEO / BYTE_LENGTH,
     &                                FRONT_END_BUSY, 
     &                                LVL1_DATA_BLOCK,
     &                                MASK)
      DO GEO_SECT = GEO_NUM_MIN, GEO_NUM_MAX
        GS_FRONT_END_BUSY(GEO_SECT) = BTEST(MASK, GEO_SECT)
      END DO
C
C       Specific Trigger Fired
C
      FIRED_MASK = L1_CRATE_HEADER(6)
      DO TRIGGER = TRG_NUM_MIN, TRG_NUM_MAX
        FIRED_TRIGGER(TRIGGER) = BTEST(FIRED_MASK, TRIGGER)
      END DO
      CALL PRTRGR_FIRST_BYTE_DECODING(NUM_TOT_TRG / BYTE_LENGTH, 
     &                                SP_TRG_FIRED, 
     &                                LVL1_DATA_BLOCK,
     &                                FIRED_MASK)
      DO TRIGGER = TRG_NUM_MIN, TRG_NUM_MAX
        ST_LEVEL1_STATE(TRIGGER) = BTEST(FIRED_MASK, TRIGGER)
      END DO
C
C       Start Digitize
C
      CALL PRTRGR_FIRST_BYTE_DECODING(NUM_TOT_GEO / BYTE_LENGTH,
     &                                START_DIGITIZE, 
     &                                LVL1_DATA_BLOCK,
     &                                MASK)
      DO GEO_SECT = GEO_NUM_MIN, GEO_NUM_MAX
        GS_STARTDGT(GEO_SECT) = BTEST(MASK, GEO_SECT)
      END DO
C
C       Front End Busy Disable
C
      CALL PRTRGR_FIRST_BYTE_DECODING(NUM_TOT_TRG / BYTE_LENGTH,
     &                                FRONT_END_DSBL, 
     &                                LVL1_DATA_BLOCK,
     &                                MASK)
      DO TRIGGER = TRG_NUM_MIN, TRG_NUM_MAX
        ST_FRONT_END_BUSY_DISABLE(TRIGGER) = BTEST(MASK, TRIGGER)
      END DO
C
C       Second Level Disable
C
      CALL PRTRGR_FIRST_BYTE_DECODING(NUM_TOT_TRG / BYTE_LENGTH,
     &                                SCND_LVL_DSBL, 
     &                                LVL1_DATA_BLOCK,
     &                                MASK)
      DO TRIGGER = TRG_NUM_MIN, TRG_NUM_MAX
        ST_L2_DISABLE(TRIGGER) = BTEST(MASK, TRIGGER)
      END DO
C
C       Energies
C
      CALL PRTRGR_FIRST_BYTE_DECODING(
     &  GLOBAL_L, EM_ET_TOTAL, 
     &  LVL1_DATA_BLOCK, GLOBAL_ENERGY(GL_EMET_THRTYP))
      CALL PRTRGR_FIRST_BYTE_DECODING(
     &  GLOBAL_L, HD_ET_TOTAL, 
     &  LVL1_DATA_BLOCK, GLOBAL_ENERGY(GL_HDET_THRTYP))
      CALL PRTRGR_FIRST_BYTE_DECODING( GLOBAL_L, PX_TOTAL, 
     &                                  LVL1_DATA_BLOCK, TOTAL_PX)
      CALL PRTRGR_FIRST_BYTE_DECODING( GLOBAL_L, PY_TOTAL, 
     &                                  LVL1_DATA_BLOCK, TOTAL_PY)
      CALL PRTRGR_FIRST_BYTE_DECODING( 
     &  GLOBAL_L, TOT_ET_TOTAL, 
     &  LVL1_DATA_BLOCK, GLOBAL_ENERGY(GL_TOTET_THRTYP))
      CALL PRTRGR_FIRST_BYTE_DECODING( 1, MPT_TOTAL, 
     &                                  LVL1_DATA_BLOCK, TOTAL_MPT)
      CALL PRTRGR_FIRST_BYTE_DECODING(
     &  GLOBAL_L, EM_L2_TOTAL, 
     &  LVL1_DATA_BLOCK, GLOBAL_ENERGY(GL_EML2_THRTYP))
      CALL PRTRGR_FIRST_BYTE_DECODING(
     &  GLOBAL_L, HD_L2_TOTAL, 
     &  LVL1_DATA_BLOCK, GLOBAL_ENERGY(GL_HDL2_THRTYP))
      CALL PRTRGR_FIRST_BYTE_DECODING(
     &  GLOBAL_L, TOT_L2_TOTAL, 
     &  LVL1_DATA_BLOCK, GLOBAL_ENERGY(GL_TOTL2_THRTYP))
C
C       Do sign extension
C
      CALL PRTRGR_SIGN_EXTEND_PT(GLOBAL_ENERGY(GL_EMET_THRTYP))
      CALL PRTRGR_SIGN_EXTEND_PT(GLOBAL_ENERGY(GL_HDET_THRTYP))
      CALL PRTRGR_SIGN_EXTEND_PT(GLOBAL_ENERGY(GL_TOTET_THRTYP))
      CALL PRTRGR_SIGN_EXTEND_PT(GLOBAL_ENERGY(GL_EML2_THRTYP))
      CALL PRTRGR_SIGN_EXTEND_PT(GLOBAL_ENERGY(GL_HDL2_THRTYP))
      CALL PRTRGR_SIGN_EXTEND_PT(GLOBAL_ENERGY(GL_TOTL2_THRTYP))
      CALL PRTRGR_SIGN_EXTEND_PT(TOTAL_PX)
      CALL PRTRGR_SIGN_EXTEND_PT(TOTAL_PY)
C
C       Counts
C
      DO REF_SET = EM_ET_REF_MIN, TOT_ET_REF_MAX
        CALL PRTRGR_FIRST_BYTE_DECODING(
     &    TOWER_COUNT_L, 
     &    HOT_TOWER_FINAL + (REF_SET - EM_ET_REF_MIN) * TOWER_COUNT_L, 
     &    LVL1_DATA_BLOCK,
     &    HOT_TOWER_COUNT(REF_SET))
      END DO
C
C       Level 0
C
      CALL L1UTIL_WHERE_WORD(FAST_VERTEX, IW, FIRST_BIT)
      IF (JBIT(LVL1_DATA_BLOCK(IW), FIRST_BIT + L0_SIGN_BIT) 
     &    .EQ. 1) THEN
        LEVEL_0_NZ = -1
      ELSE
        LEVEL_0_NZ = 0
      ENDIF
      CALL CBYT(LVL1_DATA_BLOCK(IW), FIRST_BIT, LEVEL_0_NZ, 
     &  1, L0_SIGN_BIT) 
C
      LEVEL_0_OK = BTEST(LVL1_DATA_BLOCK(IW), L0_GOOD_BIT)
C
C       Unpack the ADC bytes
C
      CALL L1UTIL_WHERE_WORD (TT_FADC, COUNT, BIT)
      CALL L1UTIL_ADC_COUNT_UNPACK(LVL1_DATA_BLOCK(COUNT), FADC_BYTE)
C      
C----------------------------------------------------------------------
  999 RETURN
      END
