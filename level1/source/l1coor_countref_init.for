      SUBROUTINE L1COOR_COUNTREF_INIT(THRTYP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialize the Hot Tower Count threshold from the
C-      parsed configuration file.
C-
C-   Inputs  : THRTYP   The type of threshold.
C-   Outputs : none
C-   Controls: none
C-
C-   Created  31-JUL-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$PARAMS:L1_CALTRIG.PARAMS'
      INCLUDE 'D0$PARAMS:L1_FRAMEWORK.PARAMS'
      INCLUDE 'D0$PARAMS:L1COOR_PARSER.PARAMS'
      INCLUDE 'D0$INC:L1COOR_PARSER.INC'
      INCLUDE 'D0$INC:L1C_REFSET_AND_COUNT_THRESHOLDS.INC'
C
      INTEGER THRTYP
      INTEGER OBJECT, COUNT
      INTEGER REF
      LOGICAL GOOD
C
C       There must be things beyond the REF declaration
      IF (LIST_TOP .LE. 1) THEN
        PARSE_STATUS = PARSE_BAD_FORMAT
        GOTO 999
      ENDIF
C
C       There must be a REF declaration
      IF ((LIST(1,OBJ_T_INDEX) .NE. PARSE_KEYWORD)
     &    .OR. (LIST(1,OBJ_INDEX) .NE. KEY_REF)) THEN
        PARSE_STATUS = PARSE_BAD_FORMAT
        GOTO 999
      ENDIF
C
C       There are only REF_MAX - REF_MIN + 1 reference sets.
      IF (LIST(1,ITEM_T_INDEX) .NE. PARSE_ASSERTED) THEN
        PARSE_STATUS = PARSE_BAD_FORMAT
        GOTO 999
      ENDIF
      IF ((LIST(1,ITEM_INDEX) .LT. RS_SET_MIN) .OR.
     &  (LIST(1,ITEM_INDEX) .GT. RS_SET_MAX)) THEN
        PARSE_STATUS = PARSE_BAD_PARAM
        GOTO 999
      ENDIF
      REF = LIST(1,ITEM_INDEX)
C
C       Shift everything in the list by one entry
      DO COUNT = 2, LIST_TOP
        LIST(COUNT-1,OBJ_INDEX) = LIST(COUNT,OBJ_INDEX)
        LIST(COUNT-1,OBJ_T_INDEX) = LIST(COUNT,OBJ_T_INDEX)
        LIST(COUNT-1,ITEM_INDEX) = LIST(COUNT,ITEM_INDEX)
        LIST(COUNT-1,ITEM_T_INDEX) = LIST(COUNT,ITEM_T_INDEX)
      END DO
      LIST_TOP = LIST_TOP - 1
C
C       Check various properties of the list
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
      CALL L1COOR_LIST_OBJ_HAS_ONE_ITEM(GOOD)
      IF (GOOD .EQV. .FALSE.) THEN
        PARSE_STATUS = PARSE_BAD_FORMAT
        GOTO 999
      ENDIF
C
      CALL L1COOR_LIST_OBJ_RANGE(TOWER_CNT_THRSH_MIN,
     &  TOWER_CNT_THRSH_MAX, GOOD)
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
C       Assign the thresholds
      DO COUNT = 1, LIST_TOP
        IF (THRTYP .EQ. TT_EMET_THRTYP) THEN
          HOT_COUNT_REF(LIST(COUNT,OBJ_INDEX),EM_ET_REF_MIN + REF)
     &    = LIST(COUNT,ITEM_INDEX)
        ELSE
          HOT_COUNT_REF(LIST(COUNT,OBJ_INDEX),TOT_ET_REF_MIN + REF)
     &    = LIST(COUNT,ITEM_INDEX)
        ENDIF
      END DO
C
C----------------------------------------------------------------------
  999 RETURN
      END
