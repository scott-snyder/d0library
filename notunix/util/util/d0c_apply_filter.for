      SUBROUTINE D0C_APPLY_FILTER
     &  (LUNINP,LUNOUT,LISTING,FILTERED_LISTING,OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Filter out %FORT-I- or %FORT-W- messages which are benign.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  12-JUN-1989   Harrison B. Prosper
C-   Updated  20-FEB-1992   Harrison B. Prosper
C-      Add more filters
C-   Updated  10-MAR-1992   Harrison B. Prosper
C-      Correct intrinsic bug
C-   Updated  13-MAR-1992   Harrison B. Prosper
C-      Improve parsing
C-   Updated  17-MAR-1992   Harrison B. Prosper
C-      Correct Open statement
C-   Updated  14-APR-1992   Harrison B. Prosper  
C-      Fix code 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER       LUNINP
      INTEGER       LUNOUT
      CHARACTER*(*) LISTING
      CHARACTER*(*) FILTERED_LISTING
      LOGICAL       OK, DEBUG
C
      CHARACTER*(*) FORT_KEY,  PROG_KEY
      INTEGER       LFORT_KEY, LPROG_KEY, MAXREC, FIRST_COL, MAXINT
      INTEGER COLUMN1, MAXEXT
      PARAMETER( FORT_KEY = '%FORT-I-EXT' )
      PARAMETER( PROG_KEY = 'PROGRAM SECTION' )
      PARAMETER( MAXREC   = 100 )
      PARAMETER( MAXINT   =  50 )
      PARAMETER( MAXEXT   =  50 )
C
      CHARACTER*8   INTRINSIC(MAXINT)
      CHARACTER*16  EXTENSION(MAXEXT)
      CHARACTER*132 STRING, PREV_RECORD, CURR_RECORD, NEXT_RECORD
      CHARACTER*3200 COMM_RECORD
C
      LOGICAL ACTIVE,OUTPUT,EOF,NEW,FIND_INTRINSIC,SCAN_RECORDS
      LOGICAL FIND_EXTENSION, STATEMENT
      INTEGER ERROR_COUNT,VALUE_TYPE, COLUMN6, COLUMN7,LENCOMM
      INTEGER II,JJ,NN,LL,I,J,K,L,N,LF,NINT,NEXT,NLINE
      INTEGER TRULEN,LINE_NUMBER
      INTEGER LINT(MAXINT),LEXT(MAXEXT)
      REAL    A,VALUEX
C----------------------------------------------------------------------
      LOGICAL FIRST, FIRST_LINE
      SAVE FIRST, FIRST_LINE
      DATA FIRST      /.TRUE./
      DATA FIRST_LINE /.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL TRNLNM('D0CHECK$DEBUG',STRING,L)
        DEBUG = .NOT. (STRING(1:L) .EQ. 'D0CHECK$DEBUG')
C
C ****  Allowed Intrinsics
C
        INTRINSIC(1) = 'IAND'
        INTRINSIC(2) = 'IOR'
        INTRINSIC(3) = 'IEOR'
        INTRINSIC(4) = 'NOT'
        INTRINSIC(5) = 'ISHFT'
        INTRINSIC(6) = 'ISHFTC'
        INTRINSIC(7) = 'IBITS'
        INTRINSIC(8) = 'IBSET'
        INTRINSIC(9) = 'IBCLR'
        INTRINSIC(10)= 'BTEST'
        INTRINSIC(11)= 'MVBITS'
        NINT = 11  ! Number of allowed intrinsics
        DO I =  1, NINT
          LINT(I) = TRULEN(INTRINSIC(I))
        ENDDO
C
C ****  Allowed statement extensions
C
        EXTENSION(1) = 'DO WHILE'
        EXTENSION(2) = 'ENDDO'
        EXTENSION(3) = 'END DO'
        EXTENSION(4) = 'DO'
        EXTENSION(5) = 'INCLUDE'
        EXTENSION(6) = 'IMPLICIT NONE'
        EXTENSION(7) = 'BYTE'
        EXTENSION(8) = 'INTEGER*2'
        EXTENSION(9) = 'INTEGER*4'
        NEXT = 9
        DO I =  1, NEXT
          LEXT(I) = TRULEN(EXTENSION(I))
        ENDDO
      ENDIF
C
      LFORT_KEY = LEN(FORT_KEY)
      LPROG_KEY = LEN(PROG_KEY)
      LF        = LFORT_KEY
      ERROR_COUNT = 0
C
C ****  Loop over listing
C
      CURR_RECORD = ' '
      COMM_RECORD(1:1) = ' '
      LENCOMM = 1
C
      ACTIVE = .TRUE.
      DO WHILE ( ACTIVE )
C
        OUTPUT = .TRUE.
C
C ****  Note previous record
C
        PREV_RECORD = CURR_RECORD
C
C ****  Read in next record excluding possible
C ****  form-feed and page header
C
        CALL D0C_NEXT_RECORD (LUNINP,CURR_RECORD,EOF)
        IF ( EOF ) GOTO 900
C
C ****  Determine start of statements
C
        IF ( FIRST_LINE ) THEN
          FIRST_LINE = .FALSE.
          L = VALUEX(CURR_RECORD,I,J,K)
          COLUMN1 = J + 5   
          COLUMN6   = COLUMN1 + 5
          COLUMN7   = COLUMN1 + 6
        ENDIF
C
C ****  Extract FORTRAN instruction: Look for blank in columns
C ****  1 and 6
C
        STATEMENT = CURR_RECORD(COLUMN1:COLUMN1) .EQ. ' '
        NEW       = CURR_RECORD(COLUMN6:COLUMN6) .EQ. ' '
C
C ****  Could be a FORMAT or CONTINUE statement
C
        IF ( NEW ) THEN
          IF (INDEX(CURR_RECORD(COLUMN7:),'FORMAT') .GT. 0 ) THEN
            STATEMENT = .TRUE.
          ELSEIF(INDEX(CURR_RECORD(COLUMN7:),'CONTINUE') .GT. 0 ) THEN
            STATEMENT = .TRUE.
          ENDIF
        ENDIF
C
        IF ( STATEMENT ) THEN
C
C ****  Exclude in-line comments
C
          I = INDEX(CURR_RECORD,'!')
          IF ( I .LE. 0 ) THEN
            I = COLUMN7 + 65
          ELSE
            I = I - 1
          ENDIF
C
          STRING = CURR_RECORD(COLUMN7:I)
          CALL SWORDS(STRING,I,J,K)
C
          IF ( NEW ) THEN
            IF ( DEBUG ) THEN
              L = MIN(LENCOMM,132)
              WRITE(1,'(1X,A)') COMM_RECORD(1:L)
            ENDIF
            COMM_RECORD = STRING(I:J)
            LENCOMM = J - I + 1
          ELSE
            COMM_RECORD = COMM_RECORD(1:LENCOMM)//STRING(I:J)
            LENCOMM = LENCOMM + J - I + 1
          ENDIF
        ENDIF
C
C ****  Look for %FORT key-word
C
        IF ( CURR_RECORD(1:LF) .EQ. FORT_KEY ) THEN
C
C ****  Read in next record excluding possible
C ****  form-feed and page header and skip blank line
C
          CALL D0C_NEXT_RECORD (LUNINP,NEXT_RECORD,EOF)
          IF ( EOF ) GOTO 900
          CALL D0C_NEXT_RECORD (LUNINP,STRING,EOF)
          IF ( EOF ) GOTO 900
C
C ****  Make Comparisons case insensitive
C
          CALL UPCASE(COMM_RECORD(1:LENCOMM),COMM_RECORD(1:LENCOMM))
C
C ****  Filter %FORT-I-EXT_STMT
C
          IF     ( CURR_RECORD(LF+1:LF+5) .EQ. '_STMT'  ) THEN
C
C ****  Scan FORTRAN statement for a valid extension
C
            J = 0
            FIND_EXTENSION = .TRUE.
            DO WHILE ( FIND_EXTENSION )
              J = J + 1
              I = INDEX(COMM_RECORD(1:LENCOMM),EXTENSION(J)(1:LEXT(J)))
              FIND_EXTENSION = (J .LT. NEXT) .AND. (I .LE. 0)
            ENDDO
C
            IF ( I .GT. 0 ) THEN
C
C ****  Exclude TLB includes ( INCLUDE '(xxxxx)')
C
              IF ( EXTENSION(J) .EQ. 'INCLUDE' ) THEN
                K = INDEX(COMM_RECORD(1:LENCOMM),'''(')
                IF ( K .LE. 0 ) THEN
                  OUTPUT = .FALSE.
                ELSE
                  OUTPUT = .TRUE.
                ENDIF
              ENDIF
C
              OUTPUT = .FALSE.
            ELSE
              OUTPUT = .TRUE.
            ENDIF
C
          ELSEIF ( CURR_RECORD(LF+1:LF+5) .EQ. '_NAME'  ) THEN
            OUTPUT = .FALSE.
          ELSEIF ( CURR_RECORD(LF+1:LF+4) .EQ. '_COM'   ) THEN
            OUTPUT = .FALSE.
          ELSEIF ( CURR_RECORD(LF+1:LF+7) .EQ. 'BADCONT') THEN
            OUTPUT = .FALSE.
C
C ****  Allow valid intrinsics
C
          ELSEIF ( CURR_RECORD(LF+1:LF+6) .EQ. 'INTRIN' ) THEN
C
            J = 0
            FIND_INTRINSIC = .TRUE.
            DO WHILE ( FIND_INTRINSIC )
              J = J + 1
              I = INDEX(COMM_RECORD(1:LENCOMM),INTRINSIC(J)(1:LINT(J)))
              FIND_INTRINSIC = (J .LT. NINT) .AND. (I .LE. 0)
            ENDDO
C
            IF ( I .GT. 0 ) THEN
C
C ****  Exclude intrinsics prefixed with a "J"
C
              IF ( COMM_RECORD(I-1:I-1) .NE. 'J' ) THEN
                OUTPUT = .FALSE.
              ELSE
                OUTPUT = .TRUE.
              ENDIF
            ELSE
              OUTPUT = .TRUE.
            ENDIF
C
C ****  Allow O and Z format specifiers; for time being do something
C ****  very simple
C
          ELSEIF ( CURR_RECORD(LF+1:LF+4) .EQ. '_FMT'  ) THEN
C
            I = INDEX(COMM_RECORD(1:LENCOMM),'O')
            IF ( I .LE. 0 ) THEN
              I = INDEX(COMM_RECORD(1:LENCOMM),'Z')
            ENDIF
C
            IF ( I .GT. 0 ) THEN
              OUTPUT = .FALSE.
            ELSE
              OUTPUT = .TRUE.
            ENDIF
C
C ****  Allow lower case source
C
          ELSEIF ( CURR_RECORD(LF+1:LF+6) .EQ. '_SOURC'  ) THEN
            K = INDEX(CURR_RECORD,'lower case')
            IF ( K .GT. 0 ) THEN
              OUTPUT = .FALSE.
            ELSE
              OUTPUT = .TRUE.
            ENDIF
          ELSE
            OUTPUT = .TRUE.
          ENDIF

          IF ( OUTPUT ) THEN
            ERROR_COUNT = ERROR_COUNT + 1
            CALL SWORDS (CURR_RECORD,II,NN,LL)
            WRITE (UNIT=LUNOUT,FMT='(A)') CURR_RECORD(1:NN)
            CALL SWORDS (CURR_RECORD,II,NN,LL)
            WRITE (UNIT=LUNOUT,FMT='(A)') NEXT_RECORD(1:NN)
            WRITE (UNIT=LUNOUT,FMT='(A)') ' '
          ENDIF
C
        ELSE
C
C ****  Write out lines "as is"
C
          IF ( CURR_RECORD(1:5) .EQ. '%FORT' ) THEN
            ERROR_COUNT = ERROR_COUNT + 1
            OUTPUT = .TRUE.
          ELSE
            A = VALUEX (CURR_RECORD(1:4),I,J,VALUE_TYPE)
            OUTPUT = VALUE_TYPE .EQ. 1
          ENDIF
C
          IF ( OUTPUT ) THEN
            CALL SWORDS (CURR_RECORD,II,NN,LL)
            WRITE (UNIT=LUNOUT,FMT='(A)') CURR_RECORD(1:NN)
          ENDIF
C
C ****  Warn about DOUBLE COMPLEX and OPEN statement
C
          OUTPUT = .FALSE.
          IF (
     &      INDEX(COMM_RECORD(1:LENCOMM),'DOUBLE COMPLEX') .NE. 0 ) THEN
            OUTPUT = .TRUE.
            NEXT_RECORD = FORT_KEY//'_STMT, DOUBLE COMPLEX not allowed'
            ERROR_COUNT = ERROR_COUNT + 1
          ELSE
C
C ****  Check for token OPEN followed by a "("
C
            I = INDEX(COMM_RECORD(1:LENCOMM),'OPEN')
            IF ( I .GT. 0 ) THEN
C
C ****  Only blanks allowed between OPEN and "("
C
              J = INDEX(COMM_RECORD(I:LENCOMM),'(')
              IF ( J .GT. 0 ) THEN
                J = I - 1 + J
                IF ( J .GT. (I+4) ) THEN
                  CALL WORD(COMM_RECORD(I+4:J-1),II,JJ,NN)
                  IF ( NN .GT. 0 ) THEN
                    I = 0 ! Not a valid OPEN statement
                  ENDIF
                ENDIF
              ELSE
                I = 0 ! Not a valid OPEN statement
              ENDIF
            ENDIF
C
            IF ( I .GT. 0 ) THEN
              IF ( COMM_RECORD(I-2:I-1) .EQ. 'D0' ) THEN
                OUTPUT = .FALSE.
              ELSE
                OUTPUT = .TRUE.
                NEXT_RECORD = FORT_KEY//'_OPEN, Use D0OPEN'
                ERROR_COUNT = ERROR_COUNT + 1
              ENDIF
            ENDIF
          ENDIF
C
          IF ( OUTPUT ) THEN
            CALL SWORDS (NEXT_RECORD,II,NN,LL)
            WRITE (UNIT=LUNOUT,FMT='(A)') NEXT_RECORD(1:NN)
          ENDIF
        ENDIF
C
C ****  Exit loop when PROGRAM SECTIONS keywords reached
C
        ACTIVE = CURR_RECORD(1:LPROG_KEY) .NE. PROG_KEY

        IF ( .NOT. ACTIVE ) THEN
          CALL D0C_BACKSPACE(LUNINP)
        ENDIF

      ENDDO

  900 CONTINUE
      OK = ERROR_COUNT .LE. 0

  999 RETURN
      END
