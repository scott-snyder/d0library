      SUBROUTINE L1COOR_LIST_OBJ_LE_ONE_ITEM(GOOD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Check that all the objects in the list have at most 
C-   one item. Actually, check that each object has an empty parenthesis, and
C-   that each object occurs only once in the list.
C-
C-   Inputs  : none
C-   Outputs : GOOD     Whether the list passes this assertion
C-   Controls: none
C-
C-   Created   5-NOV-1991   Philippe Laurens, Steven Klocek
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C-
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
        IF (LIST(COUNT,ITEM_T_INDEX) .EQ. PARSE_NO_PAREN) THEN
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
