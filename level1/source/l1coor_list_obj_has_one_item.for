      SUBROUTINE L1COOR_LIST_OBJ_HAS_ONE_ITEM(GOOD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Check that all the objects in the list have one
C-   and only one item. Actually, check that each object has an item, and that
C-   each object occurs only once in the list.
C-
C-   Inputs  : none
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
      INTEGER COUNT, COUNT2
C
      GOOD = .TRUE.
      DO COUNT = 1, LIST_TOP
        IF ((LIST(COUNT,ITEM_T_INDEX) .EQ. PARSE_NO_PAREN)
     &    .OR. (LIST(COUNT,ITEM_T_INDEX) .EQ. PARSE_EMPTY_PAREN)) THEN
          GOOD = .FALSE.
          GOTO 999
        ENDIF
        DO COUNT2 = COUNT+1, LIST_TOP
          IF (LIST(COUNT,OBJ_INDEX) .EQ. LIST(COUNT2,OBJ_INDEX)) THEN
            GOOD = .FALSE.
            GOTO 999
          ENDIF
        END DO
      END DO
C----------------------------------------------------------------------
  999 RETURN
      END
