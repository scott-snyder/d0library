      SUBROUTINE J4RGET(CODE, VAL1, VAL2, VAL3, VAL4)
C----------------------------------------------------------------------
C-
CD   Purpose and Methods : To return four floating point values describing
CD                         some aspect of the current internal environment
CD                         of DI3000.
C-
C-   Inputs  : CODE
C-   Outputs : VAL1, VAL2, VAL3, VAL4
C-   Controls: None
C-
C-   Created   2-MAY-1989   SHAHRIAR ABACHI
C-
C----------------------------------------------------------------------
C
C    Common blocks:
CB      GRFPAR-R
C
C    Calls:
CC      ERROR.
C
C
C    Next is the declaration of parameters passed to the subroutine/function.
C
      IMPLICIT NONE
      REAL     VAL1, VAL2, VAL3, VAL4
      INTEGER  CODE
C
C    Then local declarations of variables (non-common variables).
C
C
C    Then common block declarations.
C
      INCLUDE 'D0$INC:GRFPAR.INC/LIST'
C
C    Then executable code follows
C
      IF(CODE .LT.1 .OR. CODE .GT. 3) THEN
        CALL ERROR(' CODE IS NOT IN THE RANGE: 1 ---> 3')
        GOTO 999
      ENDIF
C
      IF (CODE .EQ. 1) THEN
        VAL1 = UWIND(1)
        VAL2 = UWIND(2)
        VAL3 = UWIND(3)
        VAL4 = UWIND(4)
      ELSEIF(CODE .EQ. 2) THEN
        VAL1 = UVIEW(1)
        VAL2 = UVIEW(2)
        VAL3 = UVIEW(3)
        VAL4 = UVIEW(4)
      ELSEIF(CODE .EQ. 3) THEN
        VAL1 = UVIEW(1)
        VAL2 = UVIEW(2)
        VAL3 = UVIEW(3)
        VAL4 = UVIEW(4)
      ENDIF
C
  999 RETURN
      END
