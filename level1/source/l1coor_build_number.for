      SUBROUTINE L1COOR_BUILD_NUMBER(NUMBER,NUM_ASSERTED,GOOD)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Build a number from characters on the input stream.
C-
C-   Inputs  : none
C-   Outputs : NUMBER           The integer found
C-             NUM_ASSERTED     Whether the number was asserted or negated
C-             GOOD             Whether a valid integer was found
C-   Controls: none
C-
C-   Created  26-JUL-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C-   Updated  15-NOV-1991   Philippe Laurens, Steven Klocek   
C-                      Modified COOR parser to take input from a string rather
C-                      than a file.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:L1COOR_PARSER.PARAMS'
      INCLUDE 'D0$INC:L1COOR_PARSER.INC'
      CHARACTER*1 L1COOR_CUR_CHAR, L1COOR_NEXT_CHAR
      EXTERNAL L1COOR_CUR_CHAR, L1COOR_NEXT_CHAR
C
      INTEGER NUMBER
      LOGICAL NUM_ASSERTED, GOOD
C
      CHARACTER*1 CUR_CHAR
      INTEGER MANY_DIGITS
C
      CUR_CHAR = L1COOR_CUR_CHAR()
C
      NUM_ASSERTED = .TRUE.
      GOOD = .TRUE.
      NUMBER = 0
C
C       see if the object is negated
      IF (CUR_CHAR .EQ. '-') THEN
        NUM_ASSERTED = .FALSE.
        CUR_CHAR = L1COOR_NEXT_CHAR()
        DO WHILE (CUR_CHAR .EQ. ' ')
          CUR_CHAR = L1COOR_NEXT_CHAR()
        END DO
        IF ((CUR_CHAR .LT. '0') .OR. (CUR_CHAR .GT. '9')) THEN
          GOOD = .FALSE.
          IF (PARSE_STATUS .EQ. PARSE_SUCCESS) THEN
            PARSE_STATUS = PARSE_BAD_FORMAT
          ENDIF
          GOTO 999
        ENDIF
      END IF
C
C       build the integer
      MANY_DIGITS = 0
      DO WHILE ((CUR_CHAR .LE. '9') .AND. (CUR_CHAR .GE. '0')) 
        MANY_DIGITS = MANY_DIGITS + 1
        IF (MANY_DIGITS .GT. 10) THEN
          GOOD = .FALSE.
          IF (PARSE_STATUS .EQ. PARSE_SUCCESS) THEN
            PARSE_STATUS = PARSE_BAD_FORMAT
          ENDIF
          GOTO 999
        ENDIF
        NUMBER = NUMBER * 10 + ICHAR(CUR_CHAR) - ICHAR('0')
        CUR_CHAR = L1COOR_NEXT_CHAR()
      END DO
C
C       scan off spaces
      DO WHILE (CUR_CHAR .EQ. ' ')
        CUR_CHAR = L1COOR_NEXT_CHAR()
      END DO
C----------------------------------------------------------------------
  999 RETURN
      END
