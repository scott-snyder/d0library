      SUBROUTINE L1C_BACKUP_CALTRIG_RSLT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Make a copy of the Calorimeter Trigger simulation 
C-     results for comparision later.
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
      INTEGER REFSET, COUNT, CARD, THRTYP, CMP
C
      DO REFSET = EM_ET_REF_MIN, TOT_ET_REF_MAX
        BACK_HOT_TOWER_COUNT(REFSET) = HOT_TOWER_COUNT(REFSET)
      END DO
C
      DO CARD = 1, THRD_HOT_MAX
        DO CMP = 1, CMP_PER_CARD
          DO REFSET = EM_ET_REF_MIN, TOT_ET_REF_MAX
            BACK_HOT_TOWER_CMP_RSLT(CMP,CARD,REFSET) =
     &        HOT_TOWER_CMP_RSLT(CMP,CARD,REFSET)
          END DO
        END DO
      END DO
C
      DO COUNT = 1, 4
        DO THRTYP = GL_EMET_THRTYP, GL_TOTL2_THRTYP
          DO CARD = 1, CAT3_MAX
            BACK_GLOBAL_ENERGY_CMP_RSLT(COUNT, CARD, THRTYP) =
     &        GLOBAL_ENERGY_CMP_RSLT(COUNT, CARD, THRTYP)
          END DO
        END DO
      END DO
C
      DO THRTYP = GL_EMET_THRTYP, GL_TOTL2_THRTYP
        BACK_GLOBAL_ENERGY(THRTYP) = GLOBAL_ENERGY(THRTYP)
      END DO
C
      BACK_TOTAL_PX = TOTAL_PX
      BACK_TOTAL_PY = TOTAL_PY
      BACK_TOTAL_MPT = TOTAL_MPT
C
      DO COUNT = 1, MPT_CMP_MAX
        BACK_TOTAL_MPT_CMP_RSLT(COUNT) = TOTAL_MPT_CMP_RSLT(COUNT)
      END DO
C----------------------------------------------------------------------
  999 RETURN
      END
