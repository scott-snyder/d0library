      SUBROUTINE L1COOR_ST_PRESCALER_INIT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialize the Specific Trigger Prescalers from the
C-     parsed COOR message file.
C-
C-   Inputs  : COOR message file
C-   Outputs : Common block variable ST_PRESCALER
C-   Controls: none
C-
C-   Created   5-NOV-1991   Philippe Laurens, Steven Klocek
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
      INTEGER COUNT
      LOGICAL GOOD
      INTEGER SPECTRIG
C
      CALL L1COOR_LIST_OBJ_NOT_NEGATED(GOOD)
      IF (GOOD .EQV. .FALSE.) THEN
        PARSE_STATUS = PARSE_BAD_PARAM
        GOTO 999
      ENDIF
C
      CALL L1COOR_LIST_OBJ_RANGE(TRG_NUM_MIN, TRG_NUM_MAX, GOOD)
      IF (GOOD .EQV. .FALSE.) THEN
        PARSE_STATUS = PARSE_BAD_PARAM
        GOTO 999
      ENDIF
C
      CALL L1COOR_LIST_OBJ_LE_ONE_ITEM(GOOD)
      IF (GOOD .EQV. .FALSE.) THEN
        PARSE_STATUS = PARSE_BAD_FORMAT
        GOTO 999
      ENDIF
C
      CALL L1COOR_LIST_OBJ_NOT_KEYWORD(GOOD)
      IF (GOOD .EQV. .FALSE.) THEN
        PARSE_STATUS = PARSE_BAD_PARAM
        GOTO 999
      ENDIF
C
      CALL L1COOR_LIST_ITEM_NOT_KEYWORD(GOOD)
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
      CALL L1COOR_LIST_ITEM_NOT_NEGATED(GOOD)
      IF (GOOD .EQV. .FALSE.) THEN
        PARSE_STATUS = PARSE_BAD_PARAM
        GOTO 999
      ENDIF
C
      DO COUNT = 1, LIST_TOP
        SPECTRIG = LIST(COUNT, OBJ_INDEX)
        CALL L1COOR_ALLOC_SPECTRIG(SPECTRIG)
C
C       If the SpecTrig is followed by an empty parenthesis, set prescaler to 1
        IF (LIST(COUNT, ITEM_T_INDEX) .EQ. PARSE_EMPTY_PAREN) THEN
          ST_PRESCALER(SPECTRIG) = 1
        ELSE
          ST_PRESCALER(SPECTRIG) = LIST(COUNT,ITEM_INDEX)
        ENDIF
      END DO
C
C----------------------------------------------------------------------
  999 RETURN
      END
