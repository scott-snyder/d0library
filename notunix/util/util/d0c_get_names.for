      SUBROUTINE D0C_GET_NAMES (LUNINP, KEY, NAME, NUMBER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   For any one of the keys ENTRY, VARIABLES, ARRAYS, FUNCTIONS
C-   extract the names associated with the given key from the
C-   compilation listing.
C-
C-   Inputs  : LUNINP  [I]  Input unit of compilation listing file
C-             KEY     [C*] One of the keys:
C-
C-                          'PROGRAM'
C-                          'ENTRY'
C-                          'VARIABLES'
C-                          'ARRAYS'
C-                          'FUNCTIONS'
C-
C-   Outputs : NAME(*) [C*] Array of names
C-             NUMBER  [I]  Number of names in array
C-   Controls: None
C-
C-   Created  12-JUN-1989   Harrison B. Prosper
C-   Updated  27-FEB-1992   Harrison B. Prosper  
C-    improve 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

      INTEGER       LUNINP
      CHARACTER*(*) KEY
      CHARACTER*(*) NAME(*)
      INTEGER       NUMBER

      CHARACTER*132 RECORD

      LOGICAL ACTIVE,OUTPUT,EOF

      INTEGER LKEY,II,JJ,NN,LL,I,J,K,L,N
      INTEGER POS(20),NPOS,JJKEY
C----------------------------------------------------------------------
      INTEGER NKEYWORD
      PARAMETER( NKEYWORD = 6 )
      CHARACTER*5 KEYWORD(NKEYWORD)
      DATA KEYWORD /
     &  'PROGR',
     &  'ENTRY',
     &  'VARIA',
     &  'ARRAY',
     &  'FUNCT',
     &  'COMMA' /
C----------------------------------------------------------------------
C
C **** Get key-word ID
C
      NUMBER = 0
      JJKEY  = 0
      DO I =  1,NKEYWORD
        IF ( KEY(1:5) .EQ. KEYWORD(I) ) THEN
          JJKEY = I
          GOTO 10
        ENDIF
      ENDDO
   10 CONTINUE
      IF ( (JJKEY .LE. 0) .OR. (JJKEY .GE. NKEYWORD) ) GOTO 999
C
C ****  Scan for key-word
C
      ACTIVE = .TRUE.
      DO WHILE ( ACTIVE )

        CALL D0C_NEXT_RECORD (LUNINP,RECORD,EOF)
        IF ( EOF ) GOTO 999

        ACTIVE = RECORD(1:5) .NE. KEY(1:5)
C
C ****  If specified keyword not found BACKSPACE and EXIT
C ****  at next keyword.
C
        IF ( ACTIVE ) THEN
          DO I = JJKEY+1 ,NKEYWORD
            IF ( RECORD(1:5) .EQ. KEYWORD(I) ) THEN
              CALL D0C_BACKSPACE(LUNINP)
              GOTO 999
            ENDIF
          ENDDO
        ENDIF

      ENDDO
C
C ****  Skip blank line
C ****  Read in next record skipping over possible
C ****  form-feed and page header
C
      CALL D0C_NEXT_RECORD (LUNINP,RECORD,EOF)
      IF ( EOF ) GOTO 999
C
C ****  Get positions of string 'Name' in next record.
C
      CALL D0C_NEXT_RECORD (LUNINP,RECORD,EOF)
      IF ( EOF ) GOTO 999

      NPOS   = 0
      I      = 1
      ACTIVE = .TRUE.
      DO WHILE ( ACTIVE )
        J = INDEX(RECORD(I:),'Name')
        IF ( J .GT. 0 ) THEN
          J = I + J - 1
          I = J + 4
          NPOS = NPOS + 1
          POS(NPOS) = J
        ELSE
          ACTIVE = .FALSE.
        ENDIF
      ENDDO
C
C ****  Skip blank line
C
      CALL D0C_NEXT_RECORD (LUNINP,RECORD,EOF)
      IF ( EOF ) GOTO 999
C
C ****  Extract names
C
      NUMBER = 0
      ACTIVE = .TRUE.
      DO WHILE ( ACTIVE )

        CALL D0C_NEXT_RECORD (LUNINP,RECORD,EOF)
        IF ( EOF ) GOTO 999
        CALL SWORDS (RECORD,I,J,N)
        ACTIVE = N .GT. 0

        IF ( ACTIVE ) THEN
C
C ****  Check for keyword
C
          DO I = JJKEY+1 ,NKEYWORD
            IF ( RECORD(1:5) .EQ. KEYWORD(I) ) THEN
              CALL D0C_BACKSPACE(LUNINP)
              GOTO 999
            ENDIF
          ENDDO

          II = 0
          DO WHILE ( II .LT. NPOS .AND. ACTIVE )

            II = II + 1
            I = POS(II)
            J = INDEX(RECORD(I:),' ')

            IF ( J .GT. 1 ) THEN
              J = J + POS(II) - 2
              NUMBER = NUMBER + 1
              NAME(NUMBER) = RECORD(I:J)
            ELSE
              ACTIVE = .FALSE.
            ENDIF

          ENDDO

        ENDIF

      ENDDO

  999 RETURN
      END
