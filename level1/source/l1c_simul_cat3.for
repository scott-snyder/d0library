      SUBROUTINE L1C_SIMUL_CAT3 (NB_INPUT, INPUT,
     +                           OUT1, OUT2, CMP_RSLT,
     +                           MASK, SUB, REF)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Simulates a CAT3 card.
C-
C-   Inputs  : NB_INPUT : number of used inputs, will be limited to 8;
C-             INPUT(6) : inputs.
C-
C-   Outputs : OUT1 :        sum of the first NB_INPUT INPUT;
C-             OUT2 :        OUT1 - SUB;
C-             CMP_RSLT(4) : comparator results.
C-
C-   Controls: MASK(6) : input validation;
C-             SUB :     substractor;
C-             REF(4) :  comparators.
C-
C-   Created   6-DEC-1989   Sylvain Tisserant (MSU)
C-   Updated  23-OCT-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C-                        - Changed name of routine from TRG_SIMUL_CAT3 to
C-                        L1C_SIMUL_CAT3. 
C----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INTEGER NB_INPUT, INPUT(6), OUT1, OUT2, CMP_RSLT(4), SUB, REF(4)
      LOGICAL MASK(6)
C
      INTEGER I
C
C----------------------------------------------------------------------
C
C     Summation of at most 6 inputs
C     -----------------------------
C
      OUT1 = 0
      DO I = 1, MIN0(NB_INPUT,6)
        IF(MASK(I)) OUT1 = OUT1 + INPUT(I)
      ENDDO
      OUT2 = OUT1 - SUB
C
C     Comparisons
C     -----------
C
      DO I = 1, 4
        IF(OUT1.GE.REF(I)) THEN
          CMP_RSLT(I) = 1
        ELSE
          CMP_RSLT(I) = 0
        ENDIF
      ENDDO
C
      RETURN
      END
