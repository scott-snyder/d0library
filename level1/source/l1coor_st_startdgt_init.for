      SUBROUTINE L1COOR_ST_STARTDGT_INIT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set up the Start Digitize array from the parsed
C-      configuration file.
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created   2-AUG-1991   Level 1 Simulator, Michigan State University,
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
      INTEGER COUNT, SPEC_TRIG, GEO_SECT
      INTEGER LAST_OBJECT
      LOGICAL GOOD
C
C       Verify properties of the list
      CALL L1COOR_LIST_OBJ_HAS_PAREN(GOOD)
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
      CALL L1COOR_LIST_OBJ_NOT_NEGATED(GOOD)
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
      CALL L1COOR_LIST_OBJ_RANGE(TRG_NUM_MIN, TRG_NUM_MAX, GOOD)
      IF (GOOD .EQV. .FALSE.) THEN
        PARSE_STATUS = PARSE_BAD_PARAM
        GOTO 999
      ENDIF
C
      CALL L1COOR_LIST_ITEM_RANGE(GEO_NUM_MIN, GEO_NUM_MAX, GOOD)
      IF (GOOD .EQV. .FALSE.) THEN
        PARSE_STATUS = PARSE_BAD_PARAM
        GOTO 999
      ENDIF
C
C       Assign the values
      LAST_OBJECT = NO_OBJECT
      DO COUNT = 1, LIST_TOP
C       Clear the SpecTrig if necessary
        SPEC_TRIG = LIST(COUNT, OBJ_INDEX)
        CALL L1COOR_ALLOC_SPECTRIG(SPEC_TRIG)
        IF (SPEC_TRIG .NE. LAST_OBJECT) THEN
          DO GEO_SECT = GEO_NUM_MIN, GEO_NUM_MAX
            ST_STARTDGT(GEO_SECT, SPEC_TRIG) = .FALSE.
          END DO
        ENDIF
C
        IF (LIST(COUNT,ITEM_T_INDEX) .NE. PARSE_EMPTY_PAREN) THEN
          ST_STARTDGT(LIST(COUNT,ITEM_INDEX), SPEC_TRIG) = .TRUE.
          LAST_OBJECT = SPEC_TRIG
        ENDIF
      END DO
C----------------------------------------------------------------------
  999 RETURN
      END
