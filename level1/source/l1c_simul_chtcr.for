      SUBROUTINE L1C_SIMUL_CHTCR (CHTCR)
C----------------------------------------------------------------------
C-
C-    Purpose and Methods : Simulates the CHTCR-th CHTCR card.
C-                          The Hot Tower table building is not yet
C-                          implemented.
C-
C-    Inputs  : CHTCR : card number;
C-              CHTCR_INPUT : CFTE comparator results (common
C-              HOT_TOWER_COUNTING).
C-
C-    Outputs : CHTCR_COUNT : Hot Tower counting per card and subsection
C-              (common HOT_TOWER_COUNTING).
C-
C-    Controls: None
C-
C-    Created   6-DEC-1989   Sylvain Tisserant (MSU)
C-   Updated  23-OCT-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C-                        - Changed name of routine from TRG_SIMUL_CHTCR to
C-                          L1C_SIMUL_CHTCR. 
C-                        - Replaced D0$PARAMS:LEVEL1_CAL_TRIG.PARAMS with
C-                          D0$PARAMS:L1_CALTRIG.PARAMS 
C-                        - Replaced D0$INC:HOT_TOWER_COUNTING.INC with
C-                          D0$INC:L1C_INTERMEDIATE_COUNT.INC 
C----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$PARAMS:L1_CALTRIG.PARAMS'
      INCLUDE 'D0$INC:L1C_INTERMEDIATE_COUNT.INC'
C
      INTEGER CHTCR
C
      INTEGER SUB, BIT, COUNT
C
C----------------------------------------------------------------------
C
      DO SUB = EM_ET_REF_MIN, TOT_ET_REF_MAX
        COUNT = 0
        DO BIT = 1, 32
          IF (CHTCR_INPUT(BIT,CHTCR,SUB)) COUNT = COUNT + 1
        ENDDO
        CHTCR_COUNT(CHTCR,SUB) = COUNT
      ENDDO
      RETURN
      END
