      SUBROUTINE L1C_SWAP_CALTRIG_RSLT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Swaps the Calorimeter Trigger simulation 
C-     results with the backup made earlier. See L1C_BACKUP_CALTRIG_RSLT.
C-    
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  11-JUL-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$PARAMS:L1_CALTRIG.PARAMS'
      INCLUDE 'D0$PARAMS:L1_FRAMEWORK.PARAMS'
      INCLUDE 'D0$INC:L1C_GLOBAL_RESULTS.INC'
      INCLUDE 'D0$INC:L1C_RESULT_BACKUP.INC'
C
      INTEGER REFSET, COUNT, CARD, THRTYP, TEMP, CMP
C
      DO REFSET = EM_ET_REF_MIN, TOT_ET_REF_MAX
        TEMP = BACK_HOT_TOWER_COUNT(REFSET)
        BACK_HOT_TOWER_COUNT(REFSET) = HOT_TOWER_COUNT(REFSET)
        HOT_TOWER_COUNT(REFSET) = TEMP
      END DO
C
      DO CARD = 1, THRD_HOT_MAX
        DO CMP = 1, CMP_PER_CARD
          DO REFSET = EM_ET_REF_MIN, TOT_ET_REF_MAX
            TEMP = BACK_HOT_TOWER_CMP_RSLT(CMP,CARD,REFSET)
            BACK_HOT_TOWER_CMP_RSLT(CMP,CARD,REFSET) =
     &        HOT_TOWER_CMP_RSLT(CMP,CARD,REFSET)
            HOT_TOWER_CMP_RSLT(CMP,CARD,REFSET) = TEMP
          END DO
        END DO
      END DO
C
      DO COUNT = 1, 4
        DO THRTYP = GL_EMET_THRTYP, GL_TOTL2_THRTYP
          DO CARD = 1, CAT3_MAX
            TEMP = BACK_GLOBAL_ENERGY_CMP_RSLT(COUNT, CARD, THRTYP)
            BACK_GLOBAL_ENERGY_CMP_RSLT(COUNT, CARD, THRTYP) =
     &        GLOBAL_ENERGY_CMP_RSLT(COUNT, CARD, THRTYP)
            GLOBAL_ENERGY_CMP_RSLT(COUNT, CARD, THRTYP) = TEMP
          END DO
        END DO
      END DO
C
      DO THRTYP = GL_EMET_THRTYP, GL_TOTL2_THRTYP
        TEMP = BACK_GLOBAL_ENERGY(THRTYP)
        BACK_GLOBAL_ENERGY(THRTYP) = GLOBAL_ENERGY(THRTYP)
        GLOBAL_ENERGY(THRTYP) = TEMP
      END DO
C
      TEMP = BACK_TOTAL_PX
      BACK_TOTAL_PX = TOTAL_PX
      TOTAL_PX = TEMP
C
      TEMP = BACK_TOTAL_PY
      BACK_TOTAL_PY = TOTAL_PY
      TOTAL_PY = TEMP
C
      TEMP = BACK_TOTAL_MPT
      BACK_TOTAL_MPT = TOTAL_MPT
      TOTAL_MPT = TEMP
C
      DO COUNT = 1, MPT_CMP_MAX
        TEMP = BACK_TOTAL_MPT_CMP_RSLT(COUNT)
        BACK_TOTAL_MPT_CMP_RSLT(COUNT) = TOTAL_MPT_CMP_RSLT(COUNT)
        TOTAL_MPT_CMP_RSLT(COUNT) = TEMP
      END DO
C----------------------------------------------------------------------
  999 RETURN
      END
