      SUBROUTINE L1C_FAST_CALTRIG
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fast simulation of the 1st Level Calorimeter
C-                         Trigger.
C-
C-   Inputs  : Common blocks
C-   Outputs : Common blocks
C-   Controls: none
C-
C-   Created  16-JUL-1990   Sylvain Tisserant (MSU)
C-   Updated  12-JUL-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C-                        - Fixed bug where global threshold comparisons
C-                          are made. The comparison should be against the
C-                          sum without the correction for tree offset. The
C-                          threshold value already has the tree offset
C-                          included.
C-                        - Changed name of routine from FAST_LEVEL1_CAL_TRIG
C-                          to L1C_FAST_CALTRIG. 
C-                        - Replaced D0$PARAMS:LEVEL1_CAL_TRIG.PARAMS with
C-                          D0$PARAMS:L1_CALTRIG.PARAMS 
C-                        - Replaced D0$PARAMS:LEVEL1_FRAMEWORK.PARAMS with
C-                          D0$PARAMS:L1_FRAMEWORK.PARAMS 
C-                        - Replaced D0$INC:GLOBAL_ENERGY_REFERENCES.INC with
C-                          D0$INC:L1C_GLOBAL_ENERGY_THRESHOLDS.INC 
C-                        - Replaced D0$INC:HOT_TOWER_REFERENCES.INC with
C-                          D0$INC:L1C_REFSET_AND_COUNT_THRESHOLDS.INC 
C-                        - Replaced D0$INC:LEVEL1_CAL_TRIG_RESULTS.INC with
C-                          D0$INC:L1C_GLOBAL_RESULTS.INC 
C-                        - Replaced D0$INC:LEVEL1_CAL_TRIG_SETUP.INC with
C-                          D0$INC:L1C_CARDS_USE.INC 
C-                        - Replaced D0$INC:LV1_Z_CORRECTED_ET.INC with
C-                          D0$INC:L1C_Z_CORRECTED_ET.INC 
C-                        - Replaced D0$INC:TRG_SIMUL_EVENT.INC with
C-                          D0$INC:L1C_EVENT.INC 
C----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$PARAMS:L1_CALTRIG.PARAMS'
      INCLUDE 'D0$PARAMS:L1_FRAMEWORK.PARAMS'
      INCLUDE 'D0$INC:LEVEL1_LOOKUP.INC'
      INCLUDE 'D0$INC:L1C_GLOBAL_ENERGY_THRESHOLDS.INC'
      INCLUDE 'D0$INC:L1C_REFSET_AND_COUNT_THRESHOLDS.INC'
      INCLUDE 'D0$INC:L1C_GLOBAL_RESULTS.INC'
      INCLUDE 'D0$INC:L1C_CARDS_USE.INC'
      INCLUDE 'D0$INC:L1C_EVENT.INC'
      INCLUDE 'D0$INC:L1C_Z_CORRECTED_ET.INC'
C
      INTEGER LUQ, PHI, ETA, ETA_SIGN, I, J, CAT3, SUB, QUADRANT
      INTEGER PROM_BYTE(EM_ET_QUANT:PY_QUANT), EMT, HDT,
     +        SUM(EM_ET_QUANT:PY_QUANT)
      EQUIVALENCE (EMT,PROM_BYTE(EM_ET_QUANT)),
     +            (HDT,PROM_BYTE(HD_ET_QUANT))
      INTEGER THR_TYP, THRTYP(EM_ET_QUANT:HD_L2_QUANT)
      REAL    MPT
C
      DATA    THRTYP / GL_EMET_THRTYP, GL_EML2_THRTYP,
     +                 GL_HDET_THRTYP, GL_HDL2_THRTYP /
C
C----------------------------------------------------------------------
C
      DO LUQ = EM_ET_QUANT, PY_QUANT
        SUM(LUQ) = 0
      ENDDO
      DO SUB = EM_ET_REF_MIN, TOT_ET_REF_MAX
        HOT_TOWER_COUNT(SUB) = 0
      ENDDO
C
C Add each tower to the global sums
C
      DO ETA_SIGN = POS_ETA, NEG_ETA
        DO ETA = ETA_MIN, ETA_MAX
          DO PHI = PHI_MIN, PHI_MAX
            CALL PROM_RESPONSES (ETA_SIGN, ETA, PHI,
     +                           LEVEL_0_NZ,
     +                           FADC_BYTE(ETA_SIGN,ETA,PHI,EM_TOWER),
     +                           FADC_BYTE(ETA_SIGN,ETA,PHI,HD_TOWER),
     +                           PROM_BYTE)
C
C   Z_CORRECTED_ET is stored for access by user subroutines
C
          Z_CORRECTED_ET(ETA_SIGN, ETA, PHI, EM_TOWER)
     &      = PROM_BYTE(EM_ET_QUANT)
     &        - LOOKUP_ZERESP(ETA_SIGN, ETA, PHI, EM_ET_QUANT)
          Z_CORRECTED_ET(ETA_SIGN, ETA, PHI, HD_TOWER)
     &      = PROM_BYTE(HD_ET_QUANT)
     &        - LOOKUP_ZERESP(ETA_SIGN, ETA, PHI, HD_ET_QUANT)
C
            QUADRANT = (PHI - PHI_MIN) / 8
            IF ((QUADRANT.EQ.1).OR.(QUADRANT.EQ.2))
     +        PROM_BYTE(PX_QUANT) = -PROM_BYTE(PX_QUANT)
            IF ((QUADRANT.EQ.2).OR.(QUADRANT.EQ.3))
     +        PROM_BYTE(PY_QUANT) = -PROM_BYTE(PY_QUANT)
            DO LUQ = EM_ET_QUANT, PY_QUANT
              SUM(LUQ) = SUM(LUQ) + PROM_BYTE(LUQ)
            ENDDO
            DO SUB = EM_ET_REF_MIN, EM_ET_REF_MAX
              IF((EMT.GT.EMT_THRSHLD(ETA_SIGN,ETA,PHI,SUB))
     +           .AND.(HDT.LE.HDT_VETO(ETA_SIGN,ETA,PHI,SUB)))
     +             HOT_TOWER_COUNT(SUB) = HOT_TOWER_COUNT(SUB) + 1
            ENDDO
            DO SUB = TOT_ET_REF_MIN, TOT_ET_REF_MAX
              IF(((EMT+HDT)/2).GT.TOT_THRSHLD(ETA_SIGN,ETA,PHI,SUB))
     +             HOT_TOWER_COUNT(SUB) = HOT_TOWER_COUNT(SUB) + 1
            ENDDO
          ENDDO
        ENDDO
      ENDDO
C
C Perform Global Energy comparisons 
C
      DO LUQ = EM_ET_QUANT, HD_L2_QUANT
        IF(LUQ_PAGE_INDEX(LUQ,0).NE.0) THEN
          THR_TYP = THRTYP(LUQ)
          GLOBAL_ENERGY(THR_TYP) = SUM(LUQ) - TREE_OFFSET(LUQ)
          DO CAT3 = 1, CAT3_MAX
            DO J = 1, 4
              IF (SUM(LUQ).GE.GLOBAL_ENERGY_REF(J,CAT3,THR_TYP)) THEN
                GLOBAL_ENERGY_CMP_RSLT(J,CAT3,THR_TYP) = 1
              ELSE
                GLOBAL_ENERGY_CMP_RSLT(J,CAT3,THR_TYP) = 0
              ENDIF
            ENDDO
          ENDDO
        ENDIF
      ENDDO
C
C Calculate Total Energy sums
C
      GLOBAL_ENERGY(GL_TOTET_THRTYP) = GLOBAL_ENERGY(GL_EMET_THRTYP) +
     +                                 GLOBAL_ENERGY(GL_HDET_THRTYP)
      GLOBAL_ENERGY(GL_TOTL2_THRTYP) = GLOBAL_ENERGY(GL_EML2_THRTYP) +
     +                                 GLOBAL_ENERGY(GL_HDL2_THRTYP)
      DO THR_TYP = GL_TOTET_THRTYP, GL_TOTL2_THRTYP
        I = GLOBAL_ENERGY(THR_TYP)
        DO CAT3 = 1, CAT3_MAX
          DO J = 1, 4
            IF (I.GE.GLOBAL_ENERGY_REF(J,CAT3,THR_TYP)) THEN
              GLOBAL_ENERGY_CMP_RSLT(J,CAT3,THR_TYP) = 1
            ELSE
              GLOBAL_ENERGY_CMP_RSLT(J,CAT3,THR_TYP) = 0
            ENDIF
          ENDDO
        ENDDO
      ENDDO
C
C Calculate Missing Pt and perform Missing Pt comparisons
C
      TOTAL_PX = SUM(PX_QUANT) - TREE_OFFSET(PX_QUANT)
      TOTAL_PY = SUM(PY_QUANT) - TREE_OFFSET(PY_QUANT)
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
C Perform Hot Tower Count comparisons
C
      DO SUB = EM_ET_REF_MIN, TOT_ET_REF_MAX
        I = HOT_TOWER_COUNT(SUB)
        DO J = TOWER_CNT_THRSH_MIN, TOWER_CNT_THRSH_MAX
          IF(I.GE.HOT_COUNT_REF(J,SUB)) THEN
            HOT_TOWER_CMP_RSLT(MOD(J,CMP_PER_CARD)+1,
     &        J/CMP_PER_CARD+1,SUB) = 1
          ELSE
            HOT_TOWER_CMP_RSLT(MOD(J,CMP_PER_CARD)+1,
     &        J/CMP_PER_CARD+1,SUB) = 0
          ENDIF
        ENDDO
      ENDDO
C
      RETURN
      END
