      SUBROUTINE EXTRACT_ATTRIBUTE_LIST (
     &  LUNINP,FDB,TOKEN,TOKEN_POSN,TOKEN_LINE,TEXT_BUFFER,TEXT_LENGTH,
     &  LINES,MAX_LINES)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Extract next attribute list in a PASCAL module file. If LINES = 0
C-   then no attributes lists were found. NOTE: The file must be
C-   opened and closed externally. If LUN = 0 then fetch lines from
C-   CMS using the FDB (Fetch Data Block).
C-
C-   Inputs  : LUNINP           [I]     Input unit number
C-             FDB(5)           [I]     Fetch Data Block
C-             TOKEN            [C*]    Token identifying attribute list
C-
C-   Outputs : TOKEN_POSN       [I]     Start of token in line
C-             TOKEN_LINE       [I]     Line containing token
C-             TEXT_BUFFER(*)   [C*]    Lines containing attribute list
C-             TEXT_LENGTH(*)   [I]     Length of each line
C-             LINES            [I]     Number of lines
C-
C-   Controls: MAX_LINES        [I]     Maximum number of lines to return
C-
C-   Created  15-SEP-1989   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

      INTEGER       LUNINP
      INTEGER       FDB(5)
      CHARACTER*(*) TOKEN
      INTEGER       TOKEN_POSN
      INTEGER       TOKEN_LINE
      CHARACTER*(*) TEXT_BUFFER(*)
      INTEGER       TEXT_LENGTH(*)
      INTEGER       LINES
      INTEGER       MAX_LINES

      INTEGER       I,J,K,L,N,II,JJ,NN
      INTEGER       ISTART,IEND,COMMENT_LEN, TOKEN_LEN

      LOGICAL ACTIVE, SKIP, INHERIT_TOKEN, ENVIRONMENT_TOKEN
      LOGICAL FOUND_TOKEN, TOKEN_NOT_FOUND, SPECIAL_TOKEN
      LOGICAL COMMENT_ACTIVE, COMMENT_STARTED, COMMENT_ENDED
      LOGICAL NOT_IN_COMMENT

      CHARACTER*2   COMMENT_END
      CHARACTER*80  STRING,RECORD,OUTSTR

      INTEGER STATUS
      INTEGER CMS$FETCH_GET
      EXTERNAL CMS$_EOF
C----------------------------------------------------------------------

C ************************************************************
C ****  Find next attribute list but try to exclude those
C ****  bounded by comment markers.
C ************************************************************
      CALL WORD (TOKEN,I,J,TOKEN_LEN)

      INHERIT_TOKEN       = TOKEN(1:TOKEN_LEN) .EQ. 'INHERIT'
      ENVIRONMENT_TOKEN   = TOKEN(1:TOKEN_LEN) .EQ. 'ENVIRONMENT'
      SPECIAL_TOKEN       = INHERIT_TOKEN .OR. ENVIRONMENT_TOKEN

      TOKEN_NOT_FOUND = .TRUE.

      DO WHILE ( TOKEN_NOT_FOUND )

        COMMENT_ACTIVE = .FALSE.
        ACTIVE = .TRUE.

        DO WHILE ( ACTIVE )

          IF ( LUNINP .GT. 0 ) THEN
            READ (UNIT=LUNINP,FMT='(A)',END=800) RECORD
          ELSE
            STATUS = CMS$FETCH_GET (FDB,RECORD)
            IF ( .NOT. STATUS ) GOTO 800
          ENDIF
C
C ****  If token is INHERIT/ENVIRONMENT then exit if MODULE or PROGRAM
C ****  token found. These attributes must come before either
C ****  a MODULE or PROGRAM token.
C
          IF ( SPECIAL_TOKEN ) THEN
            CALL SWORDS (RECORD,ISTART,IEND,NN)
            CALL UPCASE (RECORD(1:IEND),STRING(1:IEND))

            I = INDEX (STRING(1:IEND),'MODULE ')
            IF ( I .LE. 0 ) THEN
              I = INDEX (STRING(1:IEND),'PROGRAM ')
            ENDIF
            IF ( I .GT. 0 ) GOTO 800
          ENDIF
C
C ****  Check for start of comment
C
          COMMENT_STARTED = .FALSE.
          IF ( .NOT. COMMENT_ACTIVE ) THEN
            II = INDEX(RECORD,'{')
            IF ( II .GT. 0 ) THEN
              COMMENT_END     = '}'
              COMMENT_ACTIVE  = .TRUE.
              COMMENT_STARTED = .TRUE.
              COMMENT_LEN     = 1
            ELSE
              II = INDEX(RECORD,'(*')
              IF ( II .GT. 0 ) THEN
                COMMENT_END     = '*)'
                COMMENT_ACTIVE  = .TRUE.
                COMMENT_STARTED = .TRUE.
                COMMENT_LEN     = 2
              ENDIF
            ENDIF
          ENDIF
C
C ****  Check for end of comment
C
          COMMENT_ENDED  = .FALSE.
          IF ( COMMENT_ACTIVE ) THEN
            JJ = INDEX(RECORD,COMMENT_END(1:COMMENT_LEN))
            IF ( JJ .GT. 0 ) THEN
              COMMENT_ACTIVE = .FALSE.
              COMMENT_ENDED  = .TRUE.
            ENDIF
          ENDIF
C
C ****  Exit loop upon finding a '['
C ****  which is preceeded neither by a ':' nor an '='.
C ****  Skip further analysis if COMMENT_ACTIVE is TRUE;
C ****  However, check if symbol '[' is bounded by comment
C ****  markers.
C
          I = INDEX (RECORD,'[')
          IF ( COMMENT_STARTED .AND. I .GT. 0 ) THEN
            NOT_IN_COMMENT = II .GT. I
          ELSE
            NOT_IN_COMMENT = .NOT. COMMENT_ACTIVE
          ENDIF

          IF ( NOT_IN_COMMENT ) THEN
            IF ( I .GT. 0 ) THEN
              J = INDEX (RECORD(1:I-1),':')
              IF ( J .LE. 0 ) THEN
                J = INDEX (RECORD(1:I-1),'=')
              ENDIF
            ENDIF
            ACTIVE = (I .EQ. 0) .OR. (J .GT. 0)
          ENDIF

        ENDDO
C
C ****  Start of attribute list has been found
C ****  Now look for TOKEN and end of attribute list
C ****  For INHERIT attribute look for ) only.
C
        LINES      = 0
        SKIP       = .TRUE.
        FOUND_TOKEN= .FALSE.
        ACTIVE     = .TRUE.

        DO WHILE ( ACTIVE )

          IF ( SKIP ) THEN              ! Skip first read
            SKIP = .FALSE.
          ELSE
            IF ( LUNINP .GT. 0 ) THEN
              READ (UNIT=LUNINP,FMT='(A)',END=800) RECORD
            ELSE
              STATUS = CMS$FETCH_GET (FDB,RECORD)
              IF ( .NOT. STATUS ) GOTO 800
            ENDIF
          ENDIF

          CALL SWORDS (RECORD,ISTART,IEND,NN)
          CALL UPCASE (RECORD(1:IEND),STRING(1:IEND))
C
C ****  If token is INHERIT/ENVIRONMENT then exit if MODULE or PROGRAM
C ****  token found. These attributes must come before either
C ****  a MODULE or PROGRAM token.
C
          IF ( SPECIAL_TOKEN ) THEN
            I = INDEX (STRING(1:IEND),'MODULE ')
            IF ( I .LE. 0 ) THEN
              I = INDEX (STRING(1:IEND),'PROGRAM ')
            ENDIF
            IF ( I .GT. 0 ) GOTO 800
          ENDIF

          LINES = LINES + 1             ! Accumulate lines
          TEXT_BUFFER(LINES) = RECORD(1:IEND)
          TEXT_LENGTH(LINES) = IEND
C
C ****  Look for token
C
          IF ( .NOT. FOUND_TOKEN ) THEN
            J = INDEX (STRING(1:IEND),TOKEN(1:TOKEN_LEN))
            FOUND_TOKEN = J .GT. 0
            IF ( FOUND_TOKEN ) THEN
              TOKEN_POSN  = J
              TOKEN_LINE  = LINES       ! Note line number
            ENDIF
          ENDIF
C
C ****  Exit loop upon finding ']' or ')'
C
          IF ( SPECIAL_TOKEN ) THEN
            ACTIVE = INDEX (RECORD(1:IEND),')') .EQ. 0
          ELSE
            ACTIVE = INDEX (RECORD(1:IEND),']') .EQ. 0
          ENDIF
        ENDDO

        TOKEN_NOT_FOUND = .NOT. FOUND_TOKEN
      ENDDO

      GOTO 999
  800 CONTINUE

      IF ( TOKEN_NOT_FOUND ) THEN
        LINES = 0
      ENDIF

  999 RETURN
C
      ENTRY EXTRACT_LAST_RECORD (OUTSTR)
      OUTSTR = STRING(1:IEND)
      RETURN
      END
