      SUBROUTINE L1COOR_GET_LIST(LINE,ERROR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read the items from a line of the COOR message file,
C-   and arrange them in an array.
C-
C-   Inputs  : LINE     The input line to parse.         
C-   Outputs : ERROR    Whether an error occured
C-   Controls: none
C-
C-   Created  25-JUL-1991   Level 1 Simulator, Michigan State University,
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
      CHARACTER*1 L1COOR_CUR_CHAR, L1COOR_NEXT_CHAR, L1COOR_INIT_CHAR
      EXTERNAL L1COOR_CUR_CHAR, L1COOR_NEXT_CHAR, L1COOR_INIT_CHAR
      INTEGER TRULEN
      EXTERNAL TRULEN
C
      CHARACTER*(*)LINE 
      LOGICAL ERROR
C
      CHARACTER*1 INPUT_CHAR
      INTEGER KEYWORD_INDEX
      INTEGER NUMBER 
      LOGICAL GOOD, NUM_ASSERTED
C
      INPUT_CHAR = L1COOR_INIT_CHAR(LINE)
C
      LIST_TOP = 0
      INPUT_CHAR = L1COOR_NEXT_CHAR()
C
C       Scan off spaces, and switch depending on the input character
C
      PARSE_STATUS = PARSE_SUCCESS
      DO WHILE (.TRUE.)
  100   CONTINUE
C
C       Scan off spaces
        INPUT_CHAR = L1COOR_CUR_CHAR()
        DO WHILE (INPUT_CHAR .EQ. ' ')
          INPUT_CHAR = L1COOR_NEXT_CHAR()
        END DO
C       Build integer from string
        IF (((INPUT_CHAR .LE. '9') .AND. (INPUT_CHAR .GE. '0')) 
     &    .OR. (INPUT_CHAR .EQ. '-')) THEN
          CALL L1COOR_BUILD_NUMBER(NUMBER,NUM_ASSERTED,GOOD)
          IF (GOOD .EQV. .TRUE.) THEN
            LIST_TOP = LIST_TOP + 1
            IF (LIST_TOP .GT. LIST_SIZE) GOTO 500
            LIST(LIST_TOP,OBJ_INDEX) = NUMBER
            IF (NUM_ASSERTED .EQV. .TRUE.) THEN
              LIST(LIST_TOP,OBJ_T_INDEX) = PARSE_ASSERTED
            ELSE
              LIST(LIST_TOP,OBJ_T_INDEX) = PARSE_NEGATED
            ENDIF
            LIST(LIST_TOP,ITEM_T_INDEX) = PARSE_NO_PAREN
          ELSE
            GOTO 100
          ENDIF
C       If no parenthesis is found, get the next object
          IF (L1COOR_CUR_CHAR() .EQ. '(') THEN
            CALL L1COOR_GET_LIST_PAREN(GOOD)
            IF (GOOD .EQV. .FALSE.) GOTO 100
          ENDIF
C
C       Build a keyword from the string
        ELSEIF (((INPUT_CHAR .LE. 'Z') .AND. (INPUT_CHAR .GE. 'A')) 
     &      .OR. ( INPUT_CHAR .EQ. '_')) THEN
          CALL L1COOR_BUILD_KEYWORD(KEYWORD_INDEX,GOOD)
          IF (GOOD .EQV. .TRUE.) THEN
            LIST_TOP = LIST_TOP + 1
            IF (LIST_TOP .GT. LIST_SIZE) GOTO 500
            LIST(LIST_TOP,OBJ_INDEX) = KEYWORD_INDEX
            LIST(LIST_TOP,OBJ_T_INDEX) = PARSE_KEYWORD
          ELSE
            IF (PARSE_STATUS .EQ. PARSE_SUCCESS) THEN
              PARSE_STATUS = PARSE_BAD_UNKNOWN
            ENDIF
            GOTO 100
          ENDIF
C       If no parenthesis is found, get the next object
          IF (L1COOR_CUR_CHAR() .EQ. '(') THEN
            CALL L1COOR_GET_LIST_PAREN(GOOD)
            IF (GOOD .EQV. .FALSE.) GOTO 100
          ENDIF
c
CZZZ    Stand along parathesis object
c
        ELSEIF(INPUT_CHAR.EQ.'(')THEN
          LIST_TOP = LIST_TOP + 1
          LIST(LIST_TOP,OBJ_INDEX) = 0
          LIST(LIST_TOP,OBJ_T_INDEX) = PARSE_ASSERTED
          CALL L1COOR_GET_LIST_PAREN(GOOD)
          IF (GOOD .EQV. .FALSE.) GOTO 100
C
C       End of line
        ELSEIF (INPUT_CHAR .EQ. EOL) THEN
          GOTO 999
C
C       End of file
        ELSEIF (INPUT_CHAR .EQ. EOF) THEN
          GOTO 999
C
C       Error input
        ELSE
          IF (PARSE_STATUS .EQ. PARSE_SUCCESS) THEN
            PARSE_STATUS = PARSE_BAD_FORMAT
          ENDIF
          INPUT_CHAR = L1COOR_NEXT_CHAR()
        ENDIF
      END DO
C
C       Stack overflow
  500 CONTINUE
      IF (PARSE_STATUS .EQ. PARSE_SUCCESS) THEN
        PARSE_STATUS = PARSE_FAILURE
        INPUT_CHAR = L1COOR_NEXT_CHAR()
      ENDIF
C----------------------------------------------------------------------
  999 CONTINUE
      IF (PARSE_STATUS .NE. PARSE_SUCCESS) THEN
        ERROR = .TRUE.
      ELSE
        ERROR = .FALSE.
      ENDIF
C
      RETURN
      END
