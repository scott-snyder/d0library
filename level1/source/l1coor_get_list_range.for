      SUBROUTINE L1COOR_GET_LIST_RANGE(RET_GOOD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Parse a range from an input COOR message
C-
C-   Inputs  : none
C-   Outputs : GOOD     Success flag. .TRUE. indicates success.
C-   Controls: none
C-
C-   Created  29-JUL-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C-   Updated  15-NOV-1991   Philippe Laurens, Steven Klocek   
C-                      Modified COOR parser to take its input from a string
C-                      rather than a file.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:L1COOR_PARSER.PARAMS'
      INCLUDE 'D0$INC:L1COOR_PARSER.INC'
      CHARACTER*1 L1COOR_NEXT_CHAR
      EXTERNAL L1COOR_NEXT_CHAR
C
      LOGICAL RET_GOOD
C
      CHARACTER*1 CUR_CHAR
      INTEGER NUMBER
      LOGICAL ASSERTED
      LOGICAL GOOD
C
      RET_GOOD = .TRUE.
C
C       Mark current item as lower boundary
      IF (LIST(LIST_TOP, ITEM_T_INDEX) .EQ. PARSE_NEGATED) THEN
        RET_GOOD = .FALSE.
        IF (PARSE_STATUS .EQ. PARSE_SUCCESS) THEN
          PARSE_STATUS = PARSE_BAD_RANGE
        ENDIF
      ENDIF
      LIST(LIST_TOP,ITEM_T_INDEX) = PARSE_LOWER_BOUNDARY
C
C       Scan off ':' and trailing spaces
      CUR_CHAR = L1COOR_NEXT_CHAR()
      DO WHILE (CUR_CHAR .EQ. ' ')
        CUR_CHAR = L1COOR_NEXT_CHAR()
      END DO
C
C       The next thing on the line should be a number
      IF (((CUR_CHAR .GE. '0') .AND. (CUR_CHAR .LE. '9'))
     &    .OR. (CUR_CHAR .EQ. '-')) THEN
        CALL L1COOR_BUILD_NUMBER(NUMBER, ASSERTED, GOOD)
        IF (GOOD .EQV. .FALSE.) THEN
          RET_GOOD = .FALSE.
          IF (PARSE_STATUS .EQ. PARSE_SUCCESS) THEN
            PARSE_STATUS = PARSE_BAD_RANGE
          ENDIF
          GOTO 999
        ENDIF
C
C       The number should be asserted
        IF (ASSERTED .NEQV. .TRUE.) THEN
          RET_GOOD = .FALSE.
          IF (PARSE_STATUS .EQ. PARSE_SUCCESS) THEN
            PARSE_STATUS = PARSE_BAD_RANGE
          ENDIF
        ENDIF
C
C       Put the number on the list
C       Duplicate the last thing on the list
        LIST_TOP = LIST_TOP + 1
        IF (LIST_TOP .GT. LIST_SIZE) THEN
          RET_GOOD = .FALSE.
          IF (PARSE_STATUS .EQ. PARSE_SUCCESS) THEN
            PARSE_STATUS = PARSE_FAILURE
          ENDIF
          GOTO 999
        ENDIF
        LIST(LIST_TOP,OBJ_INDEX) = LIST(LIST_TOP-1,OBJ_INDEX)
        LIST(LIST_TOP,OBJ_T_INDEX) = LIST(LIST_TOP-1,OBJ_T_INDEX)
        LIST(LIST_TOP,ITEM_INDEX) = NUMBER
        LIST(LIST_TOP,ITEM_T_INDEX) = PARSE_UPPER_BOUNDARY
        IF (LIST(LIST_TOP,ITEM_INDEX) 
     &      .LE. LIST(LIST_TOP-1,ITEM_INDEX)) THEN
          RET_GOOD = .FALSE.
          IF (PARSE_STATUS .EQ. PARSE_SUCCESS) THEN
            PARSE_STATUS = PARSE_BAD_RANGE
          ENDIF
        ENDIF
        GOTO 999
      ENDIF
C
C       If execution reaches here, then it was not a number which followed 
C       the colon
      RET_GOOD = .FALSE.
      IF (PARSE_STATUS .EQ. PARSE_SUCCESS) THEN
        PARSE_STATUS = PARSE_BAD_FORMAT
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
