      SUBROUTINE L1COOR_LIST_OBJ_NOT_KEYWORD(GOOD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Check that all the objects in the list are not
C-   keywords.
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
      INTEGER COUNT
C
      GOOD = .TRUE.
      DO COUNT = 1, LIST_TOP
        IF (LIST(COUNT,OBJ_T_INDEX) .EQ. PARSE_KEYWORD) THEN
          GOOD = .FALSE.
          GOTO 999
        ENDIF
      END DO
C----------------------------------------------------------------------
  999 RETURN
      END
