      SUBROUTINE L1COOR_ST_VS_RS_INIT(RS_TYPE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialize the specific trigger VS reference set
C-      from the parsed configuration file.
C-
C-   Inputs  : RS_TYPE   The type of reference set 
C-                      (EM_ET_REF_MIN or TOT_ET_REF_MIN or LT_REF_MIN
C-   Outputs : none
C-   Controls: none
C-
C-   Created  31-JUL-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C-   Updated   3-DEC-1991   Philippe Laurens, Steven Klocek   
C-                      fix bug that was wiping out other specific trigger
C-                      lists.
C-   Updated  28-SEP-1992   Philippe Laurens, Steven Klocek   
C-                      Allocate Specific Trigger
C-   Updated   6-JUL-1993   Philippe Laurens - MSU L1 Trigger  
C-                      Add Large Tile 
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
      INTEGER RS_TYPE
      INTEGER COUNT
      INTEGER SPEC_TRIG
      LOGICAL GOOD
      INTEGER LAST_OBJECT
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
      CALL L1COOR_LIST_OBJ_HAS_PAREN(GOOD)
      IF (GOOD .EQV. .FALSE.) THEN
        PARSE_STATUS = PARSE_BAD_FORMAT
      ENDIF
C
      CALL L1COOR_LIST_ITEM_NOT_NEGATED(GOOD)
      IF (GOOD .EQV. .FALSE.) THEN
        PARSE_STATUS = PARSE_BAD_PARAM
      ENDIF
C
      CALL L1COOR_LIST_ITEM_NOT_KEYWORD(GOOD)
      IF (GOOD .EQV. .FALSE.) THEN
        PARSE_STATUS = PARSE_BAD_PARAM
      ENDIF
C
      CALL L1COOR_LIST_ITEM_NOT_RANGE(GOOD)
      IF (GOOD .EQV. .FALSE.) THEN
        PARSE_STATUS = PARSE_BAD_FORMAT
      ENDIF
C
      IF ( RS_TYPE .NE. LT_REF_MIN ) THEN
        CALL L1COOR_LIST_OBJ_RANGE(RS_SET_MIN, RS_SET_MAX, GOOD)
      ELSE 
        CALL L1COOR_LIST_OBJ_RANGE( RS_SET_MIN, 
     &                              LT_REF_MAX-LT_REF_MIN, 
     &                              GOOD)
      ENDIF
      IF (GOOD .EQV. .FALSE.) THEN
        PARSE_STATUS = PARSE_BAD_PARAM
      ENDIF
C
      CALL L1COOR_LIST_ITEM_RANGE(TRG_NUM_MIN, TRG_NUM_MAX, GOOD)
      IF (GOOD .EQV. .FALSE.) THEN
        PARSE_STATUS = PARSE_BAD_PARAM
      ENDIF
C
C       Go through list, and assign values.
      LAST_OBJECT = NO_OBJECT
      DO COUNT = 1, LIST_TOP
C       Clear array if necessary
        IF (LIST(COUNT,OBJ_INDEX) .NE. LAST_OBJECT) THEN
          DO SPEC_TRIG = TRG_NUM_MIN, TRG_NUM_MAX
            ST_VS_RS(SPEC_TRIG, RS_TYPE+LIST(COUNT,OBJ_INDEX)) = .FALSE.
          END DO
        ENDIF
C       Set the array entrys
        SPEC_TRIG = LIST(COUNT,ITEM_INDEX)
        IF (LIST(COUNT,ITEM_T_INDEX) .NE. PARSE_EMPTY_PAREN) THEN
          CALL L1COOR_ALLOC_SPECTRIG(SPEC_TRIG)
          ST_VS_RS(SPEC_TRIG,RS_TYPE+LIST(COUNT,OBJ_INDEX)) = .TRUE.
        ENDIF
        LAST_OBJECT = LIST(COUNT,OBJ_INDEX)
      END DO
C
C----------------------------------------------------------------------
  999 RETURN
      END
