      SUBROUTINE L1C_SIMUL_CAT2 (NB_INPUT, INPUT,
     +                           OUT1, OUT2, CMP_RSLT,
     +                           MASK, SUB, REF)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Simulates a CAT2 card, inputs are 12-bit
C-                         troncated. 
C-
C-   Inputs  : NB_INPUT : number of used inputs, will be limited to 8;
C-             INPUT(8) : 12-bit inputs.
C-
C-   Outputs : OUT1 :        sum of the first NB_INPUT INPUT;
C-             OUT2 :        OUT1 - SUB;
C-             CMP_RSLT(4) : comparator results.
C-
C-   Controls: MASK(8) : input validation;
C-             SUB :     substractor;
C-             REF(4) :  comparators.
C-
C-   Created   6-DEC-1989   Sylvain Tisserant (MSU)
C-   Updated  23-OCT-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C-                        - Changed name of routine from TRG_SIMUL_CAT2 to
C-                        L1C_SIMUL_CAT2. 
C----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INTEGER NB_INPUT, INPUT(8), OUT1, OUT2, CMP_RSLT(4), SUB, REF(4)
      LOGICAL MASK(8)
C
      INTEGER I, TRUNC
C
C----------------------------------------------------------------------
C
C     Summation with 12-bit truncation of at most 8 inputs
C     ----------------------------------------------------
C
      OUT1 = 0
      DO I = 1, MIN0(NB_INPUT,8)
        TRUNC = INPUT(I) - ((INPUT(I)/4096)*4096)
        IF(MASK(I)) OUT1 = OUT1 + TRUNC
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
