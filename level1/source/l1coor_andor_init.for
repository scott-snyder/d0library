      SUBROUTINE L1COOR_ANDOR_INIT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialize the Specific Trigger AndOr terms from the
C-     parsed configuration file.
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  31-JUL-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C-   Updated  28-SEP-1992   Philippe Laurens, Steven Klocek   
C-                      Allocate Specific Trigger
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$PARAMS:L1_CALTRIG.PARAMS'
      INCLUDE 'D0$PARAMS:L1_FRAMEWORK.PARAMS'
      INCLUDE 'D0$PARAMS:L1COOR_PARSER.PARAMS'
      INCLUDE 'D0$INC:L1COOR_PARSER.INC'
      INCLUDE 'D0$INC:L1FW_ANDOR_AND_MISC.INC'
C
      LOGICAL GOOD
      INTEGER ANDOR, SPECTRIG, LAST_OBJECT, OBJECT
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
      CALL L1COOR_LIST_ITEM_RANGE(ANDOR_NUM_MIN, ANDOR_NUM_MAX, GOOD)
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
        SPECTRIG = LIST(OBJECT, OBJ_INDEX)
        CALL L1COOR_ALLOC_SPECTRIG(SPECTRIG)
        IF ((SPECTRIG .NE. LAST_OBJECT)
     &    .OR. (LIST(OBJECT,ITEM_T_INDEX) .EQ. PARSE_EMPTY_PAREN)) THEN
          DO ANDOR = ANDOR_NUM_MIN, ANDOR_NUM_MAX
            SPECTRIG_ANDOR_ALLOC(ANDOR, SPECTRIG) = .FALSE.
            SPECTRIG_ANDOR_POLARITY(ANDOR, SPECTRIG) = .FALSE.
          END DO
        END IF
C
C       Set up the ST AND-OR terms
        IF (LIST(OBJECT,ITEM_T_INDEX) .NE. PARSE_EMPTY_PAREN) THEN 
          SPECTRIG_ANDOR_ALLOC(LIST(OBJECT,ITEM_INDEX),
     &      LIST(OBJECT,OBJ_INDEX)) = .TRUE.
          IF (LIST(OBJECT,ITEM_T_INDEX) .EQ. PARSE_ASSERTED) THEN
            SPECTRIG_ANDOR_POLARITY(LIST(OBJECT, ITEM_INDEX), 
     &        LIST(OBJECT, OBJ_INDEX)) = .TRUE.
          ELSE
            SPECTRIG_ANDOR_POLARITY(LIST(OBJECT, ITEM_INDEX), 
     &        LIST(OBJECT, OBJ_INDEX)) = .FALSE.
          ENDIF
        ENDIF
        LAST_OBJECT = LIST(OBJECT,OBJ_INDEX)
      END DO
C----------------------------------------------------------------------
  999 RETURN
      END
