      SUBROUTINE L1C_COMPARE_CALTRIG_RSLT(PASSED_VERIFY)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Compare the results of the fast simulation with the
C-      results of the normal simulation.
C-
C-   Inputs  : PASSED_VERIFY    Whether the verification was passed or not.
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
      LOGICAL PASSED_VERIFY
      INTEGER REFSET, COUNT, CARD, THRTYP, CMP
C
      PASSED_VERIFY = .FALSE.
C
      DO REFSET = EM_ET_REF_MIN, TOT_ET_REF_MAX
        IF (BACK_HOT_TOWER_COUNT(REFSET) 
     &    .NE. HOT_TOWER_COUNT(REFSET)) GOTO 999
      END DO
C
      DO CARD = 1, THRD_HOT_MAX
        DO CMP = 1, CMP_PER_CARD
          DO REFSET = EM_ET_REF_MIN, TOT_ET_REF_MAX
            IF (BACK_HOT_TOWER_CMP_RSLT(CMP,CARD,REFSET) .NE.
     &        HOT_TOWER_CMP_RSLT(CMP,CARD,REFSET)) GOTO 999
          END DO
        END DO
      END DO
C
      DO COUNT = 1, 4
        DO THRTYP = GL_EMET_THRTYP, GL_TOTL2_THRTYP
          DO CARD = 1, CAT3_MAX
            IF (BACK_GLOBAL_ENERGY_CMP_RSLT(COUNT, CARD, THRTYP) .NE.
     &        GLOBAL_ENERGY_CMP_RSLT(COUNT, CARD, THRTYP)) GOTO 999
          END DO
        END DO
      END DO
C
      DO THRTYP = GL_EMET_THRTYP, GL_TOTL2_THRTYP
        IF (BACK_GLOBAL_ENERGY(THRTYP) 
     &    .NE. GLOBAL_ENERGY(THRTYP)) GOTO 999
      END DO
C
      IF (BACK_TOTAL_PX .NE. TOTAL_PX) GOTO 999
      IF (BACK_TOTAL_PY .NE. TOTAL_PY) GOTO 999
      IF (BACK_TOTAL_MPT .NE. TOTAL_MPT) GOTO 999
C
      DO COUNT = 1, MPT_CMP_MAX
        IF (BACK_TOTAL_MPT_CMP_RSLT(COUNT) .NE.
     &    TOTAL_MPT_CMP_RSLT(COUNT)) GOTO 999
      END DO
C
      PASSED_VERIFY = .TRUE.
C----------------------------------------------------------------------
  999 RETURN
      END
