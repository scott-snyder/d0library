      SUBROUTINE INIT_COMMON_BLOCK_MINIMAL()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initializes the arrays in the common block.
C-      Sets them to 0 if diagnostics are off, or to out of range values
C-      if diagnostics are on.
C-
C-   Inputs  :
C-   Outputs : modifies common block LEVEL1_LOOKUP
C-   Controls:
C-
C-   Created  18-JUN-1990   MICHIGAN STATE UNIVERSITY, TRIGGER CONTROL SOFTWARE
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
C
C       Global declarations
C
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$INC:LEVEL1_LOOKUP.INC'
C
C       local variables
C
      INTEGER CHANNEL
      INTEGER LUQ
      INTEGER PROM
      INTEGER SIGN_ETA
      INTEGER PHI
      INTEGER ETA
      INTEGER PAGE
      INTEGER INDEX
      INTEGER BIN
      INTEGER INTEGER_VALUE
      REAL    REAL_VALUE
C
C       Initialize a couple of values
C
      INTEGER_VALUE = 0
      REAL_VALUE = 0.0
C----------------------------------------------------------------------
C-
C-      do LEVEL_0_BINS_(LOW)|(HIGH),  LUQ_PAGE_NUMBER
C-
      DO BIN = L0_BIN_MIN, L0_BIN_MAX
        L0_BIN_COVERAGE(BIN, Z_LOW) = REAL_VALUE
        L0_BIN_COVERAGE(BIN, Z_HIGH) = REAL_VALUE
        DO LUQ = EM_ET_QUANT, PY_QUANT
          LUQ_PAGE_NUMBER( LUQ, BIN ) = INTEGER_VALUE
        END DO
      END DO
C----------------------------------------------------------------------
C-
C-      do LOOKUP_QUANTITIES, PAGE_VS_BIN, 
C-         GLOBAL_ENERGY_SCALE, TREE_OFFSET
C-
      DO LUQ = EM_ET_QUANT, PY_QUANT
        GLOBAL_ENERGY_SCALE(LUQ) = REAL_VALUE
        TREE_OFFSET( LUQ ) = REAL_VALUE ! Not read from the file, but
                                        ! probably should be init. anyway
        DO PAGE = PAGE_NUM_MIN, PAGE_NUM_MAX
          LUQ_PAGE_INDEX( LUQ, PAGE ) = INTEGER_VALUE
        END DO
      END DO
      GLOBAL_ENERGY_SCALE(TOT_ET_QUANT) = REAL_VALUE
      GLOBAL_ENERGY_SCALE(TOT_L2_QUANT) = REAL_VALUE
C----------------------------------------------------------------------
C-
C-      do GLOBAL_ADC_SCALE
C-
      GLOBAL_ADC_SCALE = REAL_VALUE
C----------------------------------------------------------------------
C-
C-      do ANALOG_INPUT_SCALING,
C-         DOWNLOADED_BYTE, ADC_ZERESP, ELECT_NOISE
C-
      DO CHANNEL = EM_TOWER, TOT_TOWER
        DO PHI = PHI_MIN, PHI_MAX
          DO ETA = ETA_RANGE_MIN, ETA_RANGE_MAX
            DO SIGN_ETA = POS_ETA, NEG_ETA
              IF (CHANNEL .NE. TOT_TOWER) THEN
                ANALOG_INPUT_SCALING(SIGN_ETA, ETA, PHI, CHANNEL) =
     &          REAL_VALUE
                DAC_BYTE(SIGN_ETA, ETA, PHI, CHANNEL) = INTEGER_VALUE
                ADC_ZERESP(SIGN_ETA, ETA, PHI, CHANNEL) = INTEGER_VALUE
                ELEC_NOISE_SIGMA(SIGN_ETA, ETA, PHI, CHANNEL) =
     &          REAL_VALUE
              ENDIF
            END DO
          END DO
        END DO
      END DO
C----------------------------------------------------------------------
C-
C-      do LOOKUP_ZERESP
C-
      DO LUQ = EM_ET_QUANT, PY_QUANT
        DO PHI = PHI_MIN, PHI_MAX
          DO ETA = ETA_RANGE_MIN, ETA_RANGE_MAX
            DO SIGN_ETA = POS_ETA, NEG_ETA
              LOOKUP_ZERESP(SIGN_ETA, ETA, PHI, LUQ) = INTEGER_VALUE
            END DO
          END DO
        END DO
      END DO
C----------------------------------------------------------------------
C-
C-      do PROM_OUTPUT_CUT, PROM_TRANSFER_COEFF
C-
      DO INDEX = PAGE_INDEX_MIN, PAGE_INDEX_MAX
        DO PROM = EM_PROM, PY_PROM
          DO PHI = PHI_MIN, PHI_MAX
            DO ETA = ETA_RANGE_MIN, ETA_RANGE_MAX
              DO SIGN_ETA = POS_ETA, NEG_ETA
                PROM_CUT(SIGN_ETA, ETA, PHI, PROM, INDEX) =
     &            INTEGER_VALUE
                PROM_SLOPE( SIGN_ETA, ETA, PHI, PROM, INDEX ) =
     &            REAL_VALUE
              END DO
            END DO
          END DO
        END DO
      END DO
C----------------------------------------------------------------------
C-
C-      Do the sections FIRST_LOOKUP_TYPE and SECOND_LOOKUP_TYPE
C-
      FIRST_LOOKUP_TYPE = INTEGER_VALUE
      SECOND_LOOKUP_TYPE = INTEGER_VALUE
C----------------------------------------------------------------------
C-
C-      
  999 RETURN
      END
