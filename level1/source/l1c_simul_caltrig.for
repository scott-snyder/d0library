      SUBROUTINE L1C_SIMUL_CALTRIG
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Card-by-card Level 1 Calorimeter Trigger
C-                         simulation.
C-
C-   Inputs  : FADC_BYTE :  FADC values (common EVENT);
C-
C-   Outputs : none
C-   Controls: none
C-
C-   Created   8-DEC-1989   Sylvain Tisserant (MSU)
C-   Updated  12-JUL-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C-                        - Fixed a problem with the global energy
C-                          calculation. The wrong output of the CAT3
C-                          routine (without the subtractor) was used as
C-                          the source of the global energy sum.
C-                        - More than 4 Tower Count Thresholds per
C-                          reference set are now supported.
C-                        - Removed recalculation of Level 0 data.
C-                        - Changed all occurances of TRG_SIMUL_CAT2 to
C-                          L1C_SIMUL_CAT2. 
C-                        - Changed all occurances of TRG_SIMUL_CAT3 to
C-                          L1C_SIMUL_CAT3. 
C-                        - Changed all occurances of TRG_SIMUL_CHTCR to
C-                          L1C_SIMUL_CHTCR. 
C-                        - Changed all occurances of TRG_SIMUL_CTFE to
C-                          L1C_SIMUL_CTFE. 
C-                        - Changed name of routine from
C-                          TRG_SIMUL_LEVEL1_CAL_TRIG to L1C_SIMUL_CALTRIG. 
C-                        - Replaced D0$PARAMS:LEVEL1_CAL_TRIG.PARAMS with
C-                          D0$PARAMS:L1_CALTRIG.PARAMS 
C-                        - Replaced D0$PARAMS:LEVEL1_FRAMEWORK.PARAMS with
C-                          D0$PARAMS:L1_FRAMEWORK.PARAMS 
C-                        - Replaced D0$INC:GLOBAL_ENERGY_FUTURE_USE.INC with
C-                          D0$INC:L1C_ENERGY_FUTURE_USE.INC 
C-                        - Replaced D0$INC:GLOBAL_ENERGY_REFERENCES.INC with
C-                          D0$INC:L1C_GLOBAL_ENERGY_THRESHOLDS.INC 
C-                        - Replaced D0$INC:GLOBAL_ENERGY_SUMMATIONS.INC with
C-                          D0$INC:L1C_INTERMEDIATE_ENERGY.INC 
C-                        - Replaced D0$INC:HOT_TOWER_COUNTING.INC with
C-                          D0$INC:L1C_INTERMEDIATE_COUNT.INC 
C-                        - Replaced D0$INC:HOT_TOWER_FUTURE_USE.INC with
C-                          D0$INC:L1C_COUNT_FUTURE_USE.INC 
C-                        - Replaced D0$INC:HOT_TOWER_REFERENCES.INC with
C-                          D0$INC:L1C_REFSET_AND_COUNT_THRESHOLDS.INC 
C-                        - Replaced D0$INC:LEVEL1_CAL_TRIG_RESULTS.INC with
C-                          D0$INC:L1C_GLOBAL_RESULTS.INC 
C-                        - Replaced D0$INC:LEVEL1_CAL_TRIG_SETUP.INC with
C-                          D0$INC:L1C_CARDS_USE.INC 
C-                        - Replaced D0$INC:LEVEL1_TRIGGER_DATA_BLOCK.INC
C-                          with D0$INC:L1DBB_DATA_BLOCK.INC 
C-                        - Replaced D0$INC:TRG_SIMUL_EVENT.INC with
C-                          D0$INC:L1C_EVENT.INC 
C-                        - Replaced
C-                          D0$PARAMS:LEVEL1_TRIGGER_DATA_BLOCK.PARAMS with
C-                          D0$PARAMS:L1DBB_DATA_BLOCK.PARAMS 
C----------------------------------------------------------------------
C
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$INC:LEVEL1_LOOKUP.INC'
      INCLUDE 'D0$PARAMS:L1_CALTRIG.PARAMS'
      INCLUDE 'D0$PARAMS:L1_FRAMEWORK.PARAMS'
      INCLUDE 'D0$INC:L1C_ENERGY_FUTURE_USE.INC'
      INCLUDE 'D0$INC:L1C_GLOBAL_ENERGY_THRESHOLDS.INC'
      INCLUDE 'D0$INC:L1C_INTERMEDIATE_ENERGY.INC'
      INCLUDE 'D0$INC:L1C_INTERMEDIATE_COUNT.INC'
      INCLUDE 'D0$INC:L1C_COUNT_FUTURE_USE.INC'
      INCLUDE 'D0$INC:L1C_REFSET_AND_COUNT_THRESHOLDS.INC'
      INCLUDE 'D0$INC:L1C_CARDS_USE.INC'
      INCLUDE 'D0$INC:L1C_GLOBAL_RESULTS.INC'
      INCLUDE 'D0$PARAMS:L1DBB_DATA_BLOCK.PARAMS'
      INCLUDE 'D0$INC:L1DBB_DATA_BLOCK.INC'
      INCLUDE 'D0$INC:L1C_EVENT.INC'
C
      INTEGER THR_TYP, THRTYP(EM_ET_QUANT:HD_L2_QUANT)
C
      INTEGER PHI, ETA, ETA_SIGN
      INTEGER CTFE, CHTCR, SUB, LUQ, CAT, CAT2, CAT3
      INTEGER N_LEFT, I, J
C
      INTEGER GLOBAL_SUM(2,GL_TOTET_THRTYP:GL_TOTL2_THRTYP)
      INTEGER TOTAL_OUT2, TOTAL_SUB(2)
      LOGICAL TOTAL_MASK(2)
      REAL    MPT
C
      DATA THRTYP / GL_EMET_THRTYP, GL_EML2_THRTYP,
     +              GL_HDET_THRTYP, GL_HDL2_THRTYP /
      DATA TOTAL_MASK /.TRUE.,.TRUE./, TOTAL_SUB /0,0/
C
C----------------------------------------------------------------------
C
C     CTFE stage
C     ==========
C                                                   Preset the Jet Pattern masks
C                                                   ----------------------------
      DO SUB = EM_ET_REF_MIN, TOT_ET_REF_MAX
        DO ETA_SIGN = POS_ETA, NEG_ETA
          DO ETA = ETA_MIN, ETA_MAX
            JET_PATTERN(ETA, ETA_SIGN, SUB) = 0
          ENDDO
        ENDDO
      ENDDO
C                                                           CTFE card simulation
C                                                           --------------------
      DO CTFE = 1, CTFE_USE
        CALL L1C_SIMUL_CTFE (CTFE)
      ENDDO
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C     Hot Tower counting tree
C     =======================
C                                                                     First tier
C                                                                     ----------
      DO CHTCR = 1, CHTCR_USE
        CALL L1C_SIMUL_CHTCR (CHTCR)
      ENDDO
C                                                                    Second tier
C                                                                    -----------
      N_LEFT = CHTCR_USE
      CHTCR = 1
      DO CAT2 = 1, SCND_HOT_USE
        DO SUB = EM_ET_REF_MIN, TOT_ET_REF_MAX
          CALL L1C_SIMUL_CAT2 (N_LEFT,
     +                         CHTCR_COUNT(CHTCR,SUB),
     +                         SCND_HOT_COUNT(CAT2,SUB),
     +                         SCND_HOT_OUT2(CAT2,SUB),
     +                         SCND_HOT_CMP_RSLT(1,CAT2,SUB),
     +                         SCND_HOT_MASK(CHTCR,SUB),
     +                         SCND_HOT_SUB(CAT2,SUB),
     +                         SCND_HOT_REF(1,CAT2,SUB)     )
        ENDDO
        CHTCR  = CHTCR + 8
        N_LEFT = N_LEFT - 8
      ENDDO
C                                                                     Third tier
C                                                                     ----------
      DO CAT2 = 1, THRD_HOT_MAX
        DO SUB = EM_ET_REF_MIN, TOT_ET_REF_MAX
          CALL L1C_SIMUL_CAT2(SCND_HOT_USE,
     +                      SCND_HOT_COUNT(1,SUB),
     +                      HOT_TOWER_COUNT(SUB),
     +                      THRD_HOT_OUT2(SUB),
     +                      HOT_TOWER_CMP_RSLT(1, CAT2, SUB),
     +                      THRD_HOT_MASK(1,SUB),
     +                      THRD_HOT_SUB(SUB),
     +                      HOT_COUNT_REF(
     +                        TOWER_CNT_THRSH_MIN+(CAT2-1)*CMP_PER_CARD,
     +                        SUB)   )
        END DO
      ENDDO
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C     Scalar Adder Trees :
C     ====================
C
      DO LUQ = EM_ET_QUANT, HD_L2_QUANT
C                                                          If looked up quantity
C                                                          ---------------------
        IF(LUQ_PAGE_INDEX(LUQ,0).NE.0) THEN
C                                                                     First Tier
C                                                                     ----------
          N_LEFT = CTFE_USE
          CTFE   = 1
          DO CAT2 = 1, FRST_SCALAR_USE
            CALL L1C_SIMUL_CAT2 (N_LEFT,
     +                           CTFE_SUM4(CTFE,LUQ),
     +                           FRST_SCALAR_SUM(CAT2,LUQ),
     +                           FRST_SCALAR_OUT2(CAT2,LUQ),
     +                           FRST_SCALAR_CMP_RSLT(1,CAT2,LUQ),
     +                           FRST_SCALAR_MASK(CTFE,LUQ),
     +                           FRST_SCALAR_SUB(CAT2,LUQ),
     +                           FRST_SCALAR_REF(1,CAT2,LUQ)      )
            CTFE   = CTFE   + 8
            N_LEFT = N_LEFT - 8
          ENDDO
C                                                                    Second Tier
C                                                                    -----------
          N_LEFT = FRST_SCALAR_USE
          CAT    = 1
          DO CAT2 = 1, SCND_SCALAR_USE
            CALL L1C_SIMUL_CAT2 (N_LEFT,
     +                           FRST_SCALAR_SUM(CAT,LUQ),
     +                           SCND_SCALAR_SUM(CAT2,LUQ),
     +                           SCND_SCALAR_OUT2(CAT2,LUQ),
     +                           SCND_SCALAR_CMP_RSLT(1,CAT2,LUQ),
     +                           SCND_SCALAR_MASK(CAT,LUQ),
     +                           SCND_SCALAR_SUB(CAT2,LUQ),
     +                           SCND_SCALAR_REF(1,CAT2,LUQ)        )
            CAT    = CAT    + 8
            N_LEFT = N_LEFT - 8
          ENDDO
C                                                                     Third Tier
C                                                                     ----------
          THR_TYP = THRTYP(LUQ)
          DO CAT3 = 1, CAT3_MAX
            CALL L1C_SIMUL_CAT3 (SCND_SCALAR_USE,
     +                           SCND_SCALAR_SUM(1,LUQ),
     +                           THRD_SCALAR_OUT2(LUQ),
     +                           GLOBAL_ENERGY(THR_TYP),
     +                           GLOBAL_ENERGY_CMP_RSLT(1,CAT3,THR_TYP),
     +                           THRD_SCALAR_MASK(1,LUQ),
     +                           TREE_OFFSET(LUQ),
     +                           GLOBAL_ENERGY_REF(1,CAT3,THR_TYP)   )
          ENDDO
        ENDIF
      ENDDO
C                                                        Total transverse energy
C                                                        -----------------------
C
      GLOBAL_SUM(1,GL_TOTET_THRTYP) = GLOBAL_ENERGY(GL_EMET_THRTYP)
      GLOBAL_SUM(2,GL_TOTET_THRTYP) = GLOBAL_ENERGY(GL_HDET_THRTYP)
      GLOBAL_SUM(1,GL_TOTL2_THRTYP) = GLOBAL_ENERGY(GL_EML2_THRTYP)
      GLOBAL_SUM(2,GL_TOTL2_THRTYP) = GLOBAL_ENERGY(GL_HDL2_THRTYP)
      DO THR_TYP = GL_TOTET_THRTYP, GL_TOTL2_THRTYP
        DO CAT3 = 1, CAT3_MAX
          CALL L1C_SIMUL_CAT3 (2,
     +                         GLOBAL_SUM(1,THR_TYP),
     +                         GLOBAL_ENERGY(THR_TYP),
     +                         TOTAL_OUT2,
     +                         GLOBAL_ENERGY_CMP_RSLT(1,CAT3,THR_TYP),
     +                         TOTAL_MASK,
     +                         TOTAL_SUB,
     +                         GLOBAL_ENERGY_REF(1,CAT3,THR_TYP)      )
        ENDDO
      ENDDO
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C     Missing Pt Adder Tree : 4 branchs
C     =================================
C                                                                     First Tier
C                                                                     ----------
      N_LEFT  = CTFE_USE/2
      DO CAT2 = 1, FRST_MPT_USE
        I = (CAT2-1)/2
        J = MOD(CAT2,2)
C                                                                    Positive Px
C                                                                    -----------
        CTFE = (32*I) + 1
        IF(J.EQ.0) CTFE = CTFE + 24
        CALL L1C_SIMUL_CAT2 (N_LEFT,
     +                       CTFE_SUM4(CTFE,PX_QUANT),
     +                       FRST_MPT_SUM(CAT2,POS_MPT,PX_QUANT),
     +                       FRST_MPT_OUT2(CAT2,POS_MPT,PX_QUANT),
     +                       FRST_MPT_CMP_RSLT(1,CAT2,POS_MPT,PX_QUANT),
     +                       FRST_MPT_MASK(CTFE,PX_QUANT),
     +                       FRST_MPT_SUB(CAT2,POS_MPT,PX_QUANT),
     +                       FRST_MPT_REF(1,CAT2,POS_MPT,PX_QUANT)     )
C
C                                                                    Negative Px
C                                                                    -----------
        CTFE = (32*I) + 9
        IF(J.EQ.0) CTFE = CTFE + 8
        CALL L1C_SIMUL_CAT2 (N_LEFT,
     +                       CTFE_SUM4(CTFE,PX_QUANT),
     +                       FRST_MPT_SUM(CAT2,NEG_MPT,PX_QUANT),
     +                       FRST_MPT_OUT2(CAT2,NEG_MPT,PX_QUANT),
     +                       FRST_MPT_CMP_RSLT(1,CAT2,NEG_MPT,PX_QUANT),
     +                       FRST_MPT_MASK(CTFE,PX_QUANT),
     +                       FRST_MPT_SUB(CAT2,NEG_MPT,PX_QUANT),
     +                       FRST_MPT_REF(1,CAT2,NEG_MPT,PX_QUANT)     )
C
C                                                                    Positive Py
C                                                                    -----------
        CTFE = (32*I) + 1
        IF(J.EQ.0) CTFE = CTFE + 8
        CALL L1C_SIMUL_CAT2 (N_LEFT,
     +                       CTFE_SUM4(CTFE,PY_QUANT),
     +                       FRST_MPT_SUM(CAT2,POS_MPT,PY_QUANT),
     +                       FRST_MPT_OUT2(CAT2,POS_MPT,PY_QUANT),
     +                       FRST_MPT_CMP_RSLT(1,CAT2,POS_MPT,PY_QUANT),
     +                       FRST_MPT_MASK(CTFE,PY_QUANT),
     +                       FRST_MPT_SUB(CAT2,POS_MPT,PY_QUANT),
     +                       FRST_MPT_REF(1,CAT2,POS_MPT,PY_QUANT)     )
C
C                                                                    Negative Py
C                                                                    -----------
        CTFE = (32*I) + 17
        IF(J.EQ.0) CTFE = CTFE + 8
        CALL L1C_SIMUL_CAT2 (N_LEFT,
     +                       CTFE_SUM4(CTFE,PY_QUANT),
     +                       FRST_MPT_SUM(CAT2,NEG_MPT,PY_QUANT),
     +                       FRST_MPT_OUT2(CAT2,NEG_MPT,PY_QUANT),
     +                       FRST_MPT_CMP_RSLT(1,CAT2,NEG_MPT,PY_QUANT),
     +                       FRST_MPT_MASK(CTFE,PY_QUANT),
     +                       FRST_MPT_SUB(CAT2,NEG_MPT,PY_QUANT),
     +                       FRST_MPT_REF(1,CAT2,NEG_MPT,PY_QUANT)     )
        N_LEFT = N_LEFT - 8
      ENDDO
C                                                                    Second Tier
C                                                                    -----------
      N_LEFT = FRST_MPT_USE
      CAT    = 1
      DO CAT2 = 1, SCND_MPT_USE
        DO LUQ = PX_QUANT, PY_QUANT
          DO I = POS_MPT, NEG_MPT
            CALL L1C_SIMUL_CAT2 (N_LEFT,
     +                           FRST_MPT_SUM(CAT,I,LUQ),
     +                           SCND_MPT_SUM(CAT2,I,LUQ),
     +                           SCND_MPT_OUT2(CAT2,I,LUQ),
     +                           SCND_MPT_CMP_RSLT(1,CAT2,I,LUQ),
     +                           SCND_MPT_MASK(CAT,I,LUQ),
     +                           SCND_MPT_SUB(CAT2,I,LUQ),
     +                           SCND_MPT_REF(1,CAT2,I,LUQ)      )
          ENDDO
        ENDDO
        N_LEFT = N_LEFT - 8
        CAT    = CAT    + 8
      ENDDO
C                                                                     Third Tier
C                                                                     ----------
      TOTAL_PX = - TREE_OFFSET(PX_QUANT)
      TOTAL_PY = - TREE_OFFSET(PY_QUANT)
      DO CAT2 = 1, SCND_MPT_USE
        TOTAL_PX = TOTAL_PX + SCND_MPT_SUM(CAT2,POS_MPT,PX_QUANT)
     +                      - SCND_MPT_SUM(CAT2,NEG_MPT,PX_QUANT)
        TOTAL_PY = TOTAL_PY + SCND_MPT_SUM(CAT2,POS_MPT,PY_QUANT)
     +                      - SCND_MPT_SUM(CAT2,NEG_MPT,PY_QUANT)
      ENDDO
      MPT = SQRT (FLOAT(TOTAL_PX)**2 + FLOAT(TOTAL_PY)**2)
      TOTAL_MPT = NINT(MPT)
      DO I = 1, MPT_CMP_MAX
        IF(MPT.GE.TOTAL_MPT_REF(I)) THEN
          TOTAL_MPT_CMP_RSLT(I) = 1
        ELSE
          TOTAL_MPT_CMP_RSLT(I) = 0
        ENDIF
      ENDDO
C
      RETURN
      END
