      SUBROUTINE L1COOR_L15TERM_INIT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialize the Level 1.5 Specific Trigger Terms 
C-     vs Specific Triggers from the parsed configuration file.
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created   3-DEC-1991   Philippe Laurens, Steven Klocek
C-   Updated  28-SEP-1992   Philippe Laurens, Steven Klocek  
C-                              Allocate Specific Trigger 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:L1_FRAMEWORK.PARAMS'
      INCLUDE 'D0$PARAMS:L15_FRAMEWORK.PARAMS'
      INCLUDE 'D0$PARAMS:L1COOR_PARSER.PARAMS'
      INCLUDE 'D0$INC:L1COOR_PARSER.INC'
      INCLUDE 'D0$INC:L15_FRAMEWORK.INC'
C
      LOGICAL GOOD
      INTEGER TERM, SPEC_TRIG, LAST_OBJECT, OBJECT
C
C       Verify several properties of the list
      CALL L1COOR_LIST_OBJ_NOT_NEGATED(GOOD)
      IF (GOOD .EQV. .FALSE.) THEN
        PARSE_STATUS = PARSE_BAD_PARAM
        GOTO 999
      ENDIF
C
      CALL L1COOR_LIST_OBJ_NOT_KEYWORD(GOOD)
      IF (GOOD .EQV. .FALSE.) THEN
        PARSE_STATUS = PARSE_BAD_PARAM
        GOTO 999
      ENDIF
C
      CALL L1COOR_LIST_OBJ_HAS_PAREN(GOOD)
      IF (GOOD .EQV. .FALSE.) THEN
        PARSE_STATUS = PARSE_BAD_FORMAT
        GOTO 999
      ENDIF
C
      CALL L1COOR_LIST_OBJ_RANGE(TRG_NUM_MIN, TRG_NUM_MAX, GOOD)
      IF (GOOD .EQV. .FALSE.) THEN
        PARSE_STATUS = PARSE_BAD_PARAM
        GOTO 999
      ENDIF
C
      CALL L1COOR_LIST_ITEM_NOT_NEGATED(GOOD)
      IF (GOOD .EQV. .FALSE.) THEN
        PARSE_STATUS = PARSE_BAD_PARAM
        GOTO 999
      ENDIF
C
      CALL L1COOR_LIST_ITEM_NOT_RANGE(GOOD)
      IF (GOOD .EQV. .FALSE.) THEN
        PARSE_STATUS = PARSE_BAD_FORMAT
        GOTO 999
      ENDIF
C
      CALL L1COOR_LIST_ITEM_NOT_KEYWORD(GOOD)
      IF (GOOD .EQV. .FALSE.) THEN
        PARSE_STATUS = PARSE_BAD_PARAM
        GOTO 999
      ENDIF
C
      CALL L1COOR_LIST_ITEM_RANGE(L15_TERM_NUM_MIN, L15_TERM_NUM_MAX, 
     &  GOOD)
      IF (GOOD .EQV. .FALSE.) THEN
        PARSE_STATUS = PARSE_BAD_PARAM
        GOTO 999
      ENDIF
C
C       Loop over all list entrys
      LAST_OBJECT = -1
      DO OBJECT = 1, LIST_TOP
C
C       Clear the specific trigger
        SPEC_TRIG = LIST(OBJECT, OBJ_INDEX)
        CALL L1COOR_ALLOC_SPECTRIG(SPEC_TRIG)
        IF ((SPEC_TRIG .NE. LAST_OBJECT)
     &    .OR. (LIST(OBJECT,ITEM_T_INDEX) 
     &          .EQ. PARSE_EMPTY_PAREN)) THEN
          DO TERM = L15_TERM_NUM_MIN, L15_TERM_NUM_MAX
            L15_TERM_ALLOCATE(TERM, SPEC_TRIG) = .FALSE.
          END DO
        END IF
C
C       Set up the ST AND-OR terms
        IF (LIST(OBJECT,ITEM_T_INDEX) .NE. PARSE_EMPTY_PAREN) THEN 
          L15_TERM_ALLOCATE(LIST(OBJECT,ITEM_INDEX), SPEC_TRIG) 
     &      = .TRUE.
        ENDIF
        LAST_OBJECT = SPEC_TRIG
      END DO
C----------------------------------------------------------------------
  999 RETURN
      END
