      SUBROUTINE FIND_TOP_SOLUTION1(IS1,IS2)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : STORE AWAY SOLUTION FOR INDEX IS1,IS2
C-   TO BE USED WHILE ITERATING
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   1-FEB-1993   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IS1,IS2
      INCLUDE 'D0$INC:KINEQ.INC'
      INCLUDE 'D0$INC:TOP_SOLNS.INC'
      DOUBLE PRECISION    DELM
C----------------------------------------------------------------------
      PHI_MIN = PHI
      DEL_MASS(IS1,IS2) = DELM
      LAMBDA_MIN(IS1,IS2) = LAMBDA
      SOL_MIN(IS1,IS2) = .TRUE.
      CALL UCOPYDD(PNUT1(1),PNUT1_MIN(1,IS1,IS2),2)
      CALL UCOPYDD(PNUT2(1),PNUT2_MIN(1,IS1,IS2),2)
      CALL UCOPYDD(W1(1,IS1),W1_MIN(1,IS1,IS2),4)
      CALL UCOPYDD(W2(1,IS2),W2_MIN(1,IS1,IS2),4)
      CALL UCOPYDD(TOP1(1,IS1),TOP1_MIN(1,IS1,IS2),5)
      CALL UCOPYDD(TOP2(1,IS2),TOP2_MIN(1,IS1,IS2),5)
  999 RETURN
      END
