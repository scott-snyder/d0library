      SUBROUTINE L1COOR_GET_LIST_PAREN(RET_GOOD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Process items inside of parenthesis
C-
C-   Inputs  : none
C-   Outputs : GOOD     Success flag.
C-   Controls: none
C-
C-   Created  26-JUL-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C-   Updated  15-NOV-1991   Philippe Laurens, Steven Klocek   
C-                      Modified COOR message parser to take its input from a
C-                      string rather than a file.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:L1COOR_PARSER.PARAMS'
      INCLUDE 'D0$INC:L1COOR_PARSER.INC'
      CHARACTER*1 L1COOR_CUR_CHAR, L1COOR_NEXT_CHAR
      EXTERNAL L1COOR_CUR_CHAR, L1COOR_NEXT_CHAR
C
      LOGICAL RET_GOOD
C
      CHARACTER*1 CUR_CHAR
      INTEGER COUNT
      LOGICAL GOOD
      INTEGER KEYWORD_INDEX, NUMBER
      LOGICAL ASSERTED
C
C       Scan off the parenthesis
      RET_GOOD = .TRUE.
      CUR_CHAR = L1COOR_NEXT_CHAR()
      DO WHILE (CUR_CHAR .EQ. ' ')
        CUR_CHAR = L1COOR_NEXT_CHAR()
      END DO
C
C       Set object flag
      LIST(LIST_TOP,ITEM_T_INDEX) = PARSE_EMPTY_PAREN
C
C
  100 CONTINUE
C       Find end paren, keyword or number
      IF (CUR_CHAR .EQ. ')') THEN
        CUR_CHAR = L1COOR_NEXT_CHAR()
        GOTO 999
      ENDIF
C
C       Processing if a Keyword is found inside the parenthesis
      IF (((CUR_CHAR .GE. 'A') .AND. (CUR_CHAR .LE. 'Z')) .OR.
     &    (CUR_CHAR .EQ. '_')) THEN
        CALL L1COOR_BUILD_KEYWORD(KEYWORD_INDEX, GOOD)
        CUR_CHAR = L1COOR_CUR_CHAR()
        IF (GOOD .NEQV. .TRUE.) THEN
          RET_GOOD = .FALSE.
          GOTO 100
        ENDIF
C       If the current item is not an empty paren, then a new slot need
        IF (LIST(LIST_TOP,ITEM_T_INDEX) .NE. PARSE_EMPTY_PAREN) THEN
          LIST_TOP = LIST_TOP + 1
          IF (LIST_TOP .GT. LIST_SIZE) THEN
            RET_GOOD = .FALSE.
            IF (PARSE_STATUS .EQ. PARSE_SUCCESS) THEN
              PARSE_STATUS = PARSE_FAILURE
            ENDIF
            GOTO 999
          ENDIF
          LIST(LIST_TOP,OBJ_INDEX) = 
     &      LIST(LIST_TOP-1,OBJ_INDEX)
          LIST(LIST_TOP,OBJ_T_INDEX) = 
     &      LIST(LIST_TOP-1,OBJ_T_INDEX)
        ENDIF
        LIST(LIST_TOP,ITEM_INDEX) = KEYWORD_INDEX
        LIST(LIST_TOP,ITEM_T_INDEX) = PARSE_KEYWORD
        GOTO 100
      ENDIF
C       Handle a number inside a parenthesis
      IF (((CUR_CHAR .GE. '0') .AND. (CUR_CHAR .LE. '9')) 
     &    .OR. (CUR_CHAR .EQ. '-')) THEN
        CALL L1COOR_BUILD_NUMBER(NUMBER, ASSERTED, GOOD)
        CUR_CHAR = L1COOR_CUR_CHAR()
        IF (GOOD .NEQV. .TRUE.) THEN
          RET_GOOD = .FALSE.
          GOTO 100
        ENDIF
C       If the top item is not an empty paren, then space must be opend up for
C       another list entry
        IF (LIST(LIST_TOP,ITEM_T_INDEX) .NE. PARSE_EMPTY_PAREN) THEN
          LIST_TOP = LIST_TOP + 1
          IF (LIST_TOP .GT. LIST_SIZE) THEN
            RET_GOOD = .FALSE.
            IF (PARSE_STATUS .EQ. PARSE_SUCCESS) THEN
              PARSE_STATUS = PARSE_FAILURE
            ENDIF
            GOTO 999
          ENDIF
          LIST(LIST_TOP,OBJ_INDEX) = 
     &      LIST(LIST_TOP-1,OBJ_INDEX)
          LIST(LIST_TOP,OBJ_T_INDEX) = 
     &      LIST(LIST_TOP-1,OBJ_T_INDEX)
        ENDIF
        LIST(LIST_TOP,ITEM_INDEX) = NUMBER
        IF (ASSERTED .EQV. .TRUE.) THEN
          LIST(LIST_TOP,ITEM_T_INDEX) = PARSE_ASSERTED
        ELSE
          LIST(LIST_TOP,ITEM_T_INDEX) = PARSE_NEGATED
        ENDIF
C
C       If the next character is a colon, then do processing for a range
        IF (CUR_CHAR .EQ. ':') THEN
          CALL L1COOR_GET_LIST_RANGE(GOOD)
          CUR_CHAR = L1COOR_CUR_CHAR()
          IF (GOOD .NEQV. .TRUE.) THEN
            RET_GOOD = .FALSE.
          ENDIF
        ENDIF
c
czzz    If the next character is a period, process for a float
        IF (CUR_CHAR .EQ. '.') THEN
          CALL L1COOR_GET_LIST_float(GOOD)
          CUR_CHAR = L1COOR_CUR_CHAR()
          IF (GOOD .NEQV. .TRUE.) THEN
            RET_GOOD = .FALSE.
          ENDIF
        ENDIF
        GOTO 100
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
