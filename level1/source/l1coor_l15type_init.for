      SUBROUTINE L1COOR_L15TYPE_INIT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialize the Level 1.5 Specific Trigger type 
C-      from the parsed configuration file.
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created   3-DEC-1991   Philippe Laurens, Steven Klocek
C-   Updated  28-SEP-1992   Philippe Laurens, Steven Klocek  
C-                              Allocate Specific Trigger. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:L1_FRAMEWORK.PARAMS'
      INCLUDE 'D0$PARAMS:L15_FRAMEWORK.PARAMS'
      INCLUDE 'D0$INC:L15_FRAMEWORK.INC'
      INCLUDE 'D0$PARAMS:L1COOR_PARSER.PARAMS'
      INCLUDE 'D0$INC:L1COOR_PARSER.INC'
C
      INTEGER COUNT
      LOGICAL GOOD
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
      ENDIF
C
C       Go through list, and assign values.
      DO COUNT = 1, LIST_TOP
        CALL L1COOR_ALLOC_SPECTRIG(LIST(COUNT, OBJ_T_INDEX))
        IF (LIST(COUNT,OBJ_T_INDEX) .EQ. PARSE_ASSERTED) THEN
          ST_IS_L15(LIST(COUNT,OBJ_INDEX)) = .TRUE.
        ELSE
          ST_IS_L15(LIST(COUNT,OBJ_INDEX)) = .FALSE.
        ENDIF
      END DO
C
C----------------------------------------------------------------------
  999 RETURN
      END
