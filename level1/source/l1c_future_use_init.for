      SUBROUTINE L1C_FUTURE_USE_INIT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initializes the HOT_TOWER_FUTURE_USE and
C-                         GLOBAL_ENERGY_FUTURE_USE commons.
C-
C-   Inputs  : None.
C-   Outputs : None.
C-   Controls: None.
C-
C-   Created   2-MAR-1990   Sylvain Tisserant (MSU)
C-   Updated  23-OCT-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C-                          - Changed name of routine from
C-                            TRG_SIMUL_FUTURE_USE_INIT to L1C_FUTURE_USE_INIT.
C-                          - Replaced D0$PARAMS:LEVEL1_CAL_TRIG.PARAMS with
C-                            D0$PARAMS:L1_CALTRIG.PARAMS 
C-                          - Replaced D0$INC:GLOBAL_ENERGY_FUTURE_USE.INC with
C-                            D0$INC:L1C_ENERGY_FUTURE_USE.INC 
C-                          - Replaced D0$INC:HOT_TOWER_FUTURE_USE.INC with
C-                            D0$INC:L1C_COUNT_FUTURE_USE.INC 
C----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$PARAMS:L1_CALTRIG.PARAMS'
      INCLUDE 'D0$INC:L1C_ENERGY_FUTURE_USE.INC'
      INCLUDE 'D0$INC:L1C_COUNT_FUTURE_USE.INC'
C
      INTEGER LUQ, CTFE, CAT, I, MPT_SIGN, SUB, CHTCR
C
C----------------------------------------------------------------------
C
      DO LUQ = EM_ET_QUANT, HD_L2_QUANT
        DO CTFE = 1, CTFE_MAX
          FRST_SCALAR_MASK(CTFE,LUQ) = .TRUE.
        ENDDO
        DO CAT = 1, FRST_SCALAR_MAX
          FRST_SCALAR_SUB(CAT,LUQ) = 0
          DO I = 1, 4
            FRST_SCALAR_REF(I,CAT,LUQ) = 0
          ENDDO
          SCND_SCALAR_MASK(CAT,LUQ) = .TRUE.
        ENDDO
        DO CAT = 1, SCND_SCALAR_MAX
          SCND_SCALAR_SUB(CAT,LUQ) = 0
          DO I = 1, 4
            SCND_SCALAR_REF(I,CAT,LUQ) = 0
          ENDDO
          THRD_SCALAR_MASK(CAT,LUQ) = .TRUE.
        ENDDO
      ENDDO
C
      DO LUQ = PX_QUANT, PY_QUANT
        DO CTFE = 1, CTFE_MAX
          FRST_MPT_MASK(CTFE,LUQ) = .TRUE.
        ENDDO
        DO MPT_SIGN = POS_MPT, NEG_MPT
          DO CAT = 1, FRST_MPT_MAX
            FRST_MPT_SUB(CAT,MPT_SIGN,LUQ) = 0
            DO I = 1, 4
              FRST_MPT_REF(I,CAT,MPT_SIGN,LUQ) = 0
            ENDDO
            SCND_MPT_MASK(CAT,MPT_SIGN,LUQ) = .TRUE.
          ENDDO
          DO CAT = 1, SCND_MPT_MAX
            SCND_MPT_SUB(CAT,MPT_SIGN,LUQ) = 0
            DO I = 1, 4
              SCND_MPT_REF(I,CAT,MPT_SIGN,LUQ) = 0
              ENDDO
          ENDDO
        ENDDO
      ENDDO
C
      DO SUB = EM_ET_REF_MIN, TOT_ET_REF_MAX
        DO CHTCR = 1, CHTCR_MAX
          SCND_HOT_MASK(CHTCR,SUB) = .TRUE.
        ENDDO
        DO CAT = 1, SCND_HOT_MAX
          SCND_HOT_SUB(CAT,SUB) = 0
          DO I = 1, 4
            SCND_HOT_REF(I,CAT,SUB) = 0
          ENDDO
          THRD_HOT_MASK(CAT,SUB) = .TRUE.
        ENDDO
        THRD_HOT_SUB(SUB) = 0
      ENDDO
C
      RETURN
      END
