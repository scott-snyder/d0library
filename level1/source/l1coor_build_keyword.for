      SUBROUTINE L1COOR_BUILD_KEYWORD(KEYWORD_INDEX, GOOD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Build a keyword from the input stream.
C-
C-   Inputs  : none
C-   Outputs : KEYWORD_INDEX    The numeric index of the keyword
C-             GOOD             Keyword lookup status
C-   Controls: none
C-
C-   Created  26-JUL-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C-   Updated  19-NOV-1991   Philippe Laurens, Steven Klocek   
C-                      Modified COOR message parser to take input from a
C-                      string rather than a file.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:L1COOR_PARSER.PARAMS'
      INCLUDE 'D0$INC:L1COOR_PARSER.INC'
      CHARACTER*1 L1COOR_CUR_CHAR, L1COOR_NEXT_CHAR
      EXTERNAL L1COOR_CUR_CHAR, L1COOR_NEXT_CHAR
C
      INTEGER KEYWORD_INDEX
      LOGICAL GOOD
C
      INTEGER COUNT
      CHARACTER*12 KEYWORD
      CHARACTER*12 KEYWORD_SET(1:MAX_NUM_KEYWORD)
      INTEGER MAX_LENGTH
      PARAMETER (MAX_LENGTH = 12)
      INTEGER CURRENT
      CHARACTER*1 CHAR
C
C       Put together keyword
      CURRENT = 1
      KEYWORD = L1COOR_CUR_CHAR()
      CHAR = L1COOR_NEXT_CHAR()
      DO WHILE (((CHAR .LE. 'Z') .AND. (CHAR .GE. 'A')) .OR.
     &          (CHAR .EQ. '_'))
        CURRENT = CURRENT + 1
        IF (CURRENT .GT. MAX_LENGTH) GOTO 500
        KEYWORD(CURRENT:CURRENT) = CHAR
        CHAR = L1COOR_NEXT_CHAR()
      END DO
C
C       Skip spaces
      DO WHILE (CHAR .EQ. ' ')
        CHAR = L1COOR_NEXT_CHAR()
      END DO
C
C       Try to match the keyword
      DO COUNT = 1, MAX_NUM_KEYWORD
        IF (KEYWORD .EQ. KEYWORD_SET(COUNT)) GOTO 100
      END DO
      IF (PARSE_STATUS .EQ. PARSE_SUCCESS) THEN
        PARSE_STATUS = PARSE_BAD_UNKNOWN
      ENDIF
      GOOD = .FALSE.
      GOTO 999
C
  100 CONTINUE
      KEYWORD_INDEX = COUNT
      GOOD = .TRUE.
      GOTO 999
C
C       Skip the rest of the keyword and trailing spaces when the keyword is
C       longer than the maximum of 12 characters.
  500 CONTINUE
      DO WHILE (((CHAR .LE. 'Z') .AND. (CHAR .GE. 'A')) .OR.
     &          (CHAR .EQ. '_'))
        CHAR = L1COOR_NEXT_CHAR()
      END DO
      DO WHILE (CHAR .EQ. ' ')
        CHAR = L1COOR_NEXT_CHAR()
      END DO
      IF (PARSE_STATUS .EQ. PARSE_SUCCESS) THEN
        PARSE_STATUS = PARSE_BAD_UNKNOWN
      ENDIF
      GOOD = .FALSE.
      GOTO 999
C
C----------------------------------------------------------------------
  999 RETURN
C
      ENTRY L1COOR_INIT_KEYWORD_TABLE()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialize the table of keywords.
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  26-JUL-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C----------------------------------------------------------------------
      KEYWORD_SET(KEY_SIGN_ETA) = 'SIGN_ETA'
      KEYWORD_SET(KEY_POS) = 'POS'
      KEYWORD_SET(KEY_NEG) = 'NEG'
      KEYWORD_SET(KEY_MAGN_ETA) = 'MAGN_ETA'
      KEYWORD_SET(KEY_PHI) = 'PHI'
      KEYWORD_SET(KEY_REF) = 'REF'
      KEYWORD_SET(KEY_CRATE) = 'CRATE'
      KEYWORD_SET(KEY_TERM)  = 'TERM'
      KEYWORD_SET(KEY_TYPE)  = 'TYPE'
      KEYWORD_SET(KEY_EM)    = 'EM'
      KEYWORD_SET(KEY_TOT)   = 'TOT'
      KEYWORD_SET(KEY_USE_TOOL) = 'USE_TOOL'
      KEYWORD_SET(KEY_WITH_PARAMS) = 'WITH_PARAMS'
      KEYWORD_SET(KEY_PASS_ONE_OF) = 'PASS_ONE_OF'
      KEYWORD_SET(KEY_SPTRG)       = 'SPTRG'
       RETURN
C----------------------------------------------------------------------
      END
