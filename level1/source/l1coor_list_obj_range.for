      SUBROUTINE L1COOR_LIST_OBJ_RANGE(MIN_RANGE, MAX_RANGE, GOOD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Check that all the objects in the list are in the
C-   given range.
C-
C-   Inputs  : MIN_RANGE        The smallest value allowed for an object
C-             MAX_RANGE        The largest value allowed for an object.
C-   Outputs : GOOD     Whether the list passes this assertion
C-   Controls: none
C-
C-   Created   2-AUG-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:L1COOR_PARSER.PARAMS'
      INCLUDE 'D0$INC:L1COOR_PARSER.INC'
C
      LOGICAL GOOD
      INTEGER MIN_RANGE, MAX_RANGE
      INTEGER COUNT
C
      GOOD = .TRUE.
      DO COUNT = 1, LIST_TOP
        IF ((LIST(COUNT,OBJ_INDEX) .GT. MAX_RANGE)
     &    .OR. (LIST(COUNT,OBJ_INDEX) .LT. MIN_RANGE)) THEN
          GOOD = .FALSE.
          GOTO 999
        ENDIF
      END DO
C----------------------------------------------------------------------
  999 RETURN
      END
