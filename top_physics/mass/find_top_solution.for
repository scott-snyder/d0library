      SUBROUTINE FIND_TOP_SOLUTION
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : FIND  SOLUTION COMBINATION WHEN
C-                         TOP MASS DIFFERENCE IS MINIMUM
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  26-JAN-1993   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:KINEQ.INC'
      INCLUDE 'D0$INC:TOP_SOLNS.INC'
      INTEGER IS1,IS2
      DOUBLE PRECISION  DELM
C----------------------------------------------------------------------
      PHI_MIN = PHI
      IF ( SOL(1) ) THEN
        DO IS1 = 1,2
          IF ( SOL(2) ) THEN
            DO IS2 = 1,2
              DELM = TOP1(5,IS1)-TOP2(5,IS2)
              IF ( ABS(DEL_MASS(IS1,IS2)).GT.ABS(DELM)) THEN
                DEL_MASS(IS1,IS2) = DELM
                LAMBDA_MIN(IS1,IS2) = LAMBDA
                SOL_MIN(IS1,IS2) = .TRUE.
                CALL UCOPYDD(PNUT1(1),PNUT1_MIN(1,IS1,IS2),2)
                CALL UCOPYDD(PNUT2(1),PNUT2_MIN(1,IS1,IS2),2)
                CALL UCOPYDD(W1(1,IS1),W1_MIN(1,IS1,IS2),4)
                CALL UCOPYDD(W2(1,IS2),W2_MIN(1,IS1,IS2),4)
                CALL UCOPYDD(TOP1(1,IS1),TOP1_MIN(1,IS1,IS2),5)
                CALL UCOPYDD(TOP2(1,IS2),TOP2_MIN(1,IS1,IS2),5)
              ENDIF
            ENDDO
          ENDIF
        ENDDO
      ENDIF
  999 RETURN
      END
