      SUBROUTINE L1COOR_LIST_OBJ_NO_PAREN(GOOD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Check that all the objects have no items or
C-   parenthesis. 
C-
C-   Inputs  : none
C-   Outputs : GOOD     Whether the list passes this assertion
C-   Controls: none
C-
C-   Created   3-DEC-1991   Philippe Laurens, Steven Klocek   
C-
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
        IF (LIST(COUNT,ITEM_T_INDEX) .NE. PARSE_NO_PAREN) THEN
          GOOD = .FALSE.
          GOTO 999
        ENDIF
      END DO
C----------------------------------------------------------------------
  999 RETURN
      END

