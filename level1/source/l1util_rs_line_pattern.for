      SUBROUTINE L1UTIL_RS_LINE_PATTERN(LINE, PATTERN, OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Check that the input line follows the syntax of the
C-     given pattern. In the pattern, all characters must be literal matches
C-     except lower case letters. Of these, 's' means 0 or more spaces, 'n'
C-     means 1 or more digits.
C-
C-      NOTE: Do not end PATTERN with 's' or 'n'
C-
C-   Inputs  : LINE     The line to check
C-             PATTERN  The pattern to check the line against
C-   Outputs : OK       Whether LINE matches the PATTERN
C-   Controls: none
C-
C-   Created  21-NOV-1991   Philippe Laurens, Steven Klocek
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      CHARACTER*(*) LINE, PATTERN
      LOGICAL OK
C
      INTEGER LINE_POS, PATTERN_POS
      INTEGER LINE_LEN, PATTERN_LEN
C
      OK = .FALSE.
C
      LINE_POS = 1
      PATTERN_POS = 1
      LINE_LEN = LEN(LINE)
      PATTERN_LEN = LEN(PATTERN)
      IF ((PATTERN(PATTERN_LEN:PATTERN_LEN) .EQ. 's') .OR.
     &  (PATTERN(PATTERN_LEN:PATTERN_LEN) .EQ. 'n')) GOTO 999
C
      DO WHILE (.TRUE.) 
        IF (PATTERN(PATTERN_POS:PATTERN_POS) .EQ. 's') THEN
          IF (LINE(LINE_POS:LINE_POS) .EQ. ' ') THEN
            LINE_POS = LINE_POS + 1
            IF (LINE_POS .GT. LINE_LEN) GOTO 999
          ELSE
            PATTERN_POS = PATTERN_POS + 1
            IF (PATTERN_POS .GT. PATTERN_LEN) GOTO 999
          ENDIF
C
        ELSEIF (PATTERN(PATTERN_POS:PATTERN_POS) .EQ. 'n') THEN
          IF ((LINE(LINE_POS:LINE_POS) .GE. '0') .AND.
     &      (LINE(LINE_POS:LINE_POS) .LE. '9')) THEN
            LINE_POS = LINE_POS + 1
            IF (LINE_POS .GT. LINE_LEN) GOTO 999
            IF ((LINE(LINE_POS:LINE_POS) .LT. '0') .OR.
     &        (LINE(LINE_POS:LINE_POS) .GT. '9')) THEN
              PATTERN_POS = PATTERN_POS + 1
              IF (PATTERN_POS .GT. PATTERN_LEN) GOTO 999
            ENDIF
          ELSE
            GOTO 999
          ENDIF
C
        ELSE
          IF (LINE(LINE_POS:LINE_POS) .NE.
     &      PATTERN(PATTERN_POS:PATTERN_POS)) GOTO 999
          LINE_POS = LINE_POS + 1
          PATTERN_POS = PATTERN_POS +1
          IF ((LINE_POS .GT. LINE_LEN) .AND. (PATTERN_POS .GT.
     &      PATTERN_LEN)) THEN
            OK = .TRUE.
            GOTO 999
          ENDIF
        ENDIF
C
        IF ((LINE_POS .GT. LINE_LEN) .OR. (PATTERN_POS .GT.
     &    PATTERN_LEN)) GOTO 999
      END DO
C
C----------------------------------------------------------------------
  999 RETURN
      END
