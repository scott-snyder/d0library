      SUBROUTINE L1C_SIMUL_CTFE (CTFE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Simulation of a given CTFE card.
C-
C-   Inputs  : CTFE :       card number;
C-             FADC_BYTE :  EMt and HDt FADC values (common EVENT);
C-             LEVEL_0_NZ : 5 bit Level 0 vertex coding (common EVENT)
C-                          is assumed to be 0 if no Level 0 validation.
C-
C-   Outputs : CTFE_SUM4 :   First energy summations (common
C-                           GLOBAL_ENERGY_SUMMATIONS);
C-             TOT_9BIT :    9-bit Et word (common FUTURE_USE);
C-             CHTCR_INPUT : comparator results (common HOT_TOWER_COUNTING);
C-             JET_PATTERN : Jet Pattern Masks (HOT_TOWER_COUNTING common).
C-
C-   Controls: EMT_THRSHLD, HDT_VETO, TOT_THRSHOLD : trigger tower
C-             thresholds and vetos (common HOT_TOWER_REFERENCES)
C-
C-   Created  20-NOV-1989   Sylvain Tisserant (MSU)
C-   Updated   8-JUL-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C-                        - Fixed way TOT Et is calculated. Was simply addition
C-                          of EM Et byte and HD Et byte, now add, divide by 2,
C-                          and truncate.
C-                        - Changed name of routine from TRG_SIMUL_CTFE to
C-                          L1C_SIMUL_CTFE. 
C-                        - Replaced D0$PARAMS:LEVEL1_CAL_TRIG.PARAMS with
C-                          D0$PARAMS:L1_CALTRIG.PARAMS 
C-                        - Replaced D0$PARAMS:LEVEL1_FRAMEWORK.PARAMS with
C-                          D0$PARAMS:L1_FRAMEWORK.PARAMS 
C-                        - Replaced D0$INC:GLOBAL_ENERGY_SUMMATIONS.INC with
C-                          D0$INC:L1C_INTERMEDIATE_ENERGY.INC 
C-                        - Replaced D0$INC:HOT_TOWER_COUNTING.INC with
C-                          D0$INC:L1C_INTERMEDIATE_COUNT.INC 
C-                        - Replaced D0$INC:HOT_TOWER_REFERENCES.INC with
C-                          D0$INC:L1C_REFSET_AND_COUNT_THRESHOLDS.INC 
C-                        - Replaced D0$INC:LEVEL1_TRIGGER_DATA_BLOCK.INC
C-                          with D0$INC:L1DBB_DATA_BLOCK.INC 
C-                        - Replaced D0$INC:LV1_Z_CORRECTED_ET.INC with
C-                          D0$INC:L1C_Z_CORRECTED_ET.INC 
C-                        - Replaced D0$INC:TRG_SIMUL_EVENT.INC with
C-                          D0$INC:L1C_EVENT.INC 
C-                        - Replaced D0$INC:TRG_SIMUL_FUTURE_USE.INC with
C-                          D0$INC:L1C_EVENT_FUTURE_USE.INC 
C-                        - Replaced
C-                          D0$PARAMS:LEVEL1_TRIGGER_DATA_BLOCK.PARAMS with
C-                          D0$PARAMS:L1DBB_DATA_BLOCK.PARAMS 
C-
C----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$PARAMS:L1_CALTRIG.PARAMS'
      INCLUDE 'D0$PARAMS:L1_FRAMEWORK.PARAMS'
      INCLUDE 'D0$INC:LEVEL1_LOOKUP.INC'
      INCLUDE 'D0$INC:L1C_INTERMEDIATE_ENERGY.INC'
      INCLUDE 'D0$INC:L1C_REFSET_AND_COUNT_THRESHOLDS.INC'
      INCLUDE 'D0$INC:L1C_INTERMEDIATE_COUNT.INC'
      INCLUDE 'D0$PARAMS:L1DBB_DATA_BLOCK.PARAMS'
      INCLUDE 'D0$INC:L1DBB_DATA_BLOCK.INC'
      INCLUDE 'D0$INC:L1C_EVENT.INC'
      INCLUDE 'D0$INC:L1C_EVENT_FUTURE_USE.INC'
      INCLUDE 'D0$INC:L1C_Z_CORRECTED_ET.INC'
C
      INTEGER    NB_PHI
      PARAMETER (NB_PHI=(PHI_MAX-PHI_MIN+1))
C
      INTEGER CTFE
C
      INTEGER EMT_PATTERN(PHI_MIN:PHI_MAX), TOT_PATTERN(PHI_MIN:PHI_MAX)
      INTEGER I, LUQ
      INTEGER PHI, ETA0, ETA_SIGN, CHTCR, ETA, SUB
      INTEGER EMT_CHTCR_BIT, TOT_CHTCR_BIT, EMT_JET_BIT, TOT_JET_BIT
      INTEGER PROM_BYTE(EM_ET_QUANT:PY_QUANT)
      INTEGER EMT, HDT, E_TOT
      EQUIVALENCE (EMT,PROM_BYTE(EM_ET_QUANT))
      EQUIVALENCE (HDT,PROM_BYTE(HD_ET_QUANT))
C
      DATA EMT_PATTERN /  8,  7,  6,  5,  4,  3,  2,  1,
     +                   24, 23, 22, 21, 20, 19, 18, 17,
     +                   16, 15, 14, 13, 12, 11, 10,  9,
     +                   32, 31, 30, 29, 28, 27, 26, 25 /
      DATA TOT_PATTERN /  1,  2,  3,  4,  5,  6,  7,  8,
     +                   17, 18, 19, 20, 21, 22, 23, 24,
     +                    9, 10, 11, 12, 13, 14, 15, 16,
     +                   25, 26, 27, 28, 29, 30, 31, 32 /
C
C----------------------------------------------------------------------
C
C                                                                     Sum preset
C                                                                     ----------
      DO LUQ = EM_ET_QUANT, PY_QUANT
        CTFE_SUM4(CTFE,LUQ) = 0
      ENDDO
C                                                                      Numbering
C                                                                      ---------
      I             = (CTFE-1)/NB_PHI
      PHI           = (CTFE - (I*NB_PHI)) + PHI_MIN - 1
      ETA0          = ((I/2)*4) + ETA_MIN
      ETA_SIGN      = POS_ETA + MOD(I,2)
      CHTCR         = (CTFE + 7)/8
      TOT_CHTCR_BIT = MOD((CTFE-1),8) + 1
      EMT_CHTCR_BIT = 9 - TOT_CHTCR_BIT
      EMT_JET_BIT   = EMT_PATTERN(PHI)
      TOT_JET_BIT   = TOT_PATTERN(PHI)
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C     4 trigger tower loop
C     ====================
C
      DO ETA = ETA0, ETA0+3
C                                                               LooK-up memories
C                                                               ----------------
        CALL PROM_RESPONSES (ETA_SIGN, ETA, PHI,
     +                       LEVEL_0_NZ,
     +                       FADC_BYTE (ETA_SIGN,ETA,PHI,EM_TOWER),
     +                       FADC_BYTE (ETA_SIGN,ETA,PHI,HD_TOWER),
     +                       PROM_BYTE)
        Z_CORRECTED_ET(ETA_SIGN, ETA, PHI, EM_TOWER)
     &    = PROM_BYTE(EM_ET_QUANT)
     &      - LOOKUP_ZERESP(ETA_SIGN, ETA, PHI, EM_ET_QUANT)
        Z_CORRECTED_ET(ETA_SIGN, ETA, PHI, HD_TOWER)
     &    = PROM_BYTE(HD_ET_QUANT)
     &      - LOOKUP_ZERESP(ETA_SIGN, ETA, PHI, HD_ET_QUANT)
C                                                                     Summations
C                                                                     ----------
        DO LUQ = EM_ET_QUANT, PY_QUANT
          CTFE_SUM4(CTFE,LUQ) = CTFE_SUM4(CTFE,LUQ) + PROM_BYTE(LUQ)
        ENDDO
C                                                        Comparators against ...
C                                                        -----------------------
C                                                     ... EMt and HDt references
C                                                     --------------------------
        DO SUB = EM_ET_REF_MIN, EM_ET_REF_MAX
          IF((EMT.GT.EMT_THRSHLD(ETA_SIGN,ETA,PHI,SUB))
     +       .AND.(HDT.LE.HDT_VETO(ETA_SIGN,ETA,PHI,SUB))) THEN
            CHTCR_INPUT(EMT_CHTCR_BIT,CHTCR,SUB) = .TRUE.
            CALL SBIT1(JET_PATTERN(ETA,ETA_SIGN,SUB),EMT_JET_BIT)
          ELSE
            CHTCR_INPUT(EMT_CHTCR_BIT,CHTCR,SUB) = .FALSE.
          ENDIF
        ENDDO
C                                                              ... Et references
C                                                              -----------------
        TOT_9BIT(ETA_SIGN,ETA,PHI) = EMT + HDT
C       To calculate TOT Et, add EM Et and HD Et, divide by 2 and truncate.
        E_TOT = (EMT + HDT) / 2
        DO SUB = TOT_ET_REF_MIN, TOT_ET_REF_MAX
          IF(E_TOT.GT.TOT_THRSHLD(ETA_SIGN,ETA,PHI,SUB)) THEN
            CHTCR_INPUT(TOT_CHTCR_BIT,CHTCR,SUB) = .TRUE.
            CALL SBIT1(JET_PATTERN(ETA,ETA_SIGN,SUB),TOT_JET_BIT)
          ELSE
            CHTCR_INPUT(TOT_CHTCR_BIT,CHTCR,SUB) = .FALSE.
          ENDIF
        ENDDO
C
        EMT_CHTCR_BIT = EMT_CHTCR_BIT + 8
        TOT_CHTCR_BIT = TOT_CHTCR_BIT + 8
      ENDDO
C
      RETURN
      END
