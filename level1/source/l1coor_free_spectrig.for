      SUBROUTINE L1COOR_FREE_SPECTRIG(SCALERS_ONLY)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Free the listed Specific Triggers
C-
C-   Inputs  : SCALERS_ONLY     Clear only the Specific Trigger Scalers
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
C
      INTEGER COUNT, SPEC_TRIG
      LOGICAL GOOD, SCALERS_ONLY
C
C       Check various properties of the list
C
      CALL L1COOR_LIST_OBJ_NOT_KEYWORD(GOOD)
      IF (GOOD .EQV. .FALSE.) THEN
        PARSE_STATUS = PARSE_BAD_PARAM
        GOTO 999
      ENDIF
C
      CALL L1COOR_LIST_OBJ_NO_PAREN(GOOD)
      IF (GOOD .EQV. .FALSE.) THEN
        PARSE_STATUS = PARSE_BAD_FORMAT
        GOTO 999
      ENDIF
C
      CALL L1COOR_LIST_OBJ_NOT_NEGATED(GOOD)
      IF (GOOD .EQV. .FALSE.) THEN
        PARSE_STATUS = PARSE_BAD_FORMAT
        GOTO 999
      ENDIF
C
      CALL L1COOR_LIST_OBJ_RANGE(TRG_NUM_MIN, TRG_NUM_MAX, GOOD)
      IF (GOOD .EQV. .FALSE.) THEN
        PARSE_STATUS = PARSE_BAD_PARAM
      ENDIF
C
C       Go through list, and assign values.
      DO COUNT = 1, LIST_TOP
        SPEC_TRIG = LIST(COUNT, OBJ_INDEX)
        IF (SCALERS_ONLY .EQV. .TRUE.) THEN
          CALL L1COOR_CLEAR_SPECTRIG_SCALERS(SPEC_TRIG)
        ELSE
          CALL L1COOR_DEALLOC_SPECTRIG(SPEC_TRIG)
        ENDIF
      END DO
C
C----------------------------------------------------------------------
  999 RETURN
      END
