      SUBROUTINE L1COOR_GLOBSUM_INIT(THRTYP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialize a global sum threshold from the parsed
C-      configuration file.
C-
C-   Inputs  : THRTYP   The threshold type
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
      INCLUDE 'D0$INC:L1C_GLOBAL_ENERGY_THRESHOLDS.INC'
C
      INTEGER THRTYP
      INTEGER COUNT, THRNUM
      LOGICAL GOOD
C
      CALL L1COOR_LIST_OBJ_NOT_NEGATED(GOOD)
      IF (GOOD .EQV. .FALSE.) THEN
        PARSE_STATUS = PARSE_BAD_PARAM
        GOTO 999
      ENDIF
C
      CALL L1COOR_LIST_OBJ_RANGE(SUM_MIN, SUM_MAX, GOOD)
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
        THRNUM = LIST(COUNT, OBJ_INDEX)
        CALL GLOBAL_THRESHOLD_TRANSLATION(THRTYP, 
     &    FLOAT(LIST(COUNT,ITEM_INDEX)) / 1000.,
     &    GLOBAL_ENERGY_REF(1+MOD(THRNUM, 4), 1+THRNUM/4, THRTYP))
      END DO
C----------------------------------------------------------------------
  999 RETURN
      END
