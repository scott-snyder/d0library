      SUBROUTINE L1COOR_ENABLE_SPECTRIG
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Set Specific Triggers as either enabled or disabled.
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
      LOGICAL GOOD
      INTEGER COUNT
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
      CALL L1COOR_LIST_OBJ_RANGE(TRG_NUM_MIN, TRG_NUM_MAX, GOOD)
      IF (GOOD .EQV. .FALSE.) THEN
        PARSE_STATUS = PARSE_BAD_PARAM
        GOTO 999
      ENDIF
C
C       Go through list, and assign values.
C
      DO COUNT = 1, LIST_TOP
        CALL L1COOR_ALLOC_SPECTRIG(LIST(COUNT, OBJ_INDEX))
        IF (LIST(COUNT, OBJ_T_INDEX) .EQ. PARSE_ASSERTED) THEN
          ST_ENABLED(LIST(COUNT,OBJ_INDEX)) = .TRUE.
        ELSE
          ST_ENABLED(LIST(COUNT,OBJ_INDEX)) = .FALSE.
        ENDIF
      END DO
C----------------------------------------------------------------------
  999 RETURN
      END
