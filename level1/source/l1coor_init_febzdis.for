      SUBROUTINE L1COOR_INIT_FEBZDIS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialize the Geographic Section to Specific
C-      Trigger Front End Busy matrix from the parsed configuration file.
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  17-SEP-1992   Philippe Laurens, Steven Klocek
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
        CALL L1COOR_ALLOC_SPECTRIG(LIST(COUNT, OBJ_INDEX))
        IF (LIST(COUNT,OBJ_INDEX) .NE. LAST_OBJECT) THEN
          DO GEO_SECT = GEO_NUM_MIN, GEO_NUM_MAX
            FEBUSY_GS_TO_ST(GEO_SECT, LIST(COUNT,OBJ_INDEX)) = .FALSE.
          END DO
        END IF
C
        IF (LIST(COUNT,ITEM_T_INDEX) .NE. PARSE_EMPTY_PAREN) THEN
          FEBUSY_GS_TO_ST(LIST(COUNT,ITEM_INDEX), LIST(COUNT,OBJ_INDEX))
     &      = .TRUE.
        ENDIF
      END DO
C----------------------------------------------------------------------
  999 RETURN
      END
