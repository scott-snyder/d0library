      SUBROUTINE D0C_VALIDATE_NAMES (LUNOUT, STRING, NAME, NUMBER, OK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Check length of names and check for dis-allowed characters.
C-
C-   Inputs  : LUNOUT   [I]     Unit number of error messages
C-             STRING   [C*]    String containing keywords:
C-                                      COMMON BLOCK
C-                                      VARIABLES
C-                                      ROUTINES
C-             NAME(*)  [C*]    Names to be validated
C-             NUMBER   [I]     Number of names to be validated
C-
C-   Outputs : NAME(*)  [C*]    Names with system names excluded
C-             NUMBER   [I]     Number of names
C-             OK       [L]     TRUE if ALL strings are OK.
C-   Controls:
C-
C-   Created  12-JUN-1989   Harrison B. Prosper
C-   Updated  26-FEB-1992   Harrison B. Prosper
C-    Read in list of valid routines
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

      INTEGER       LUNOUT
      CHARACTER*(*) STRING
      CHARACTER*(*) NAME(*)
      INTEGER       NUMBER
      LOGICAL       OK

      LOGICAL KEEP, ACTIVE, FOUND, ROUTINE
      INTEGER II,JJ,LL,NN,I,J,K,N,NVMSUTIL,ID

      INTEGER NWORK
      PARAMETER( NWORK = 511 )
      INTEGER WORK(NWORK)               ! Work array used by EXCLUDE_STRINGS
C
C ****  Define list of invalid symbols
C
      INTEGER MAXSYM
      PARAMETER( MAXSYM = 1)
      CHARACTER*1 SYMBOL(MAXSYM)
      DATA SYMBOL /
     &  '$' /
C
      INTEGER LUNINP
      PARAMETER( LUNINP = 50 )
C
      INTEGER MAXSYS, MAXUTIL
      PARAMETER( MAXSYS = 511)
      PARAMETER( MAXUTIL= 511)
      CHARACTER*32 SYSTEM(MAXSYS),VMSUTIL(MAXUTIL)
      CHARACTER*80 RECORD
      INTEGER IMAP(MAXUTIL)
C
C ****  Name of file containing list of allowed VMS utilities
C
      CHARACTER*(*) VMSLIST
      PARAMETER( VMSLIST = 'D0$UTIL:D0CHECK.VMSLIST' )
C----------------------------------------------------------------------
      LOGICAL FIRST
      SAVE FIRST,NVMSUTIL,VMSUTIL
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
C
C ****  Read in list of valid routine names
C
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        NVMSUTIL = 0
C
        OPEN(UNIT=LUNINP,FILE=VMSLIST,STATUS='OLD',READONLY,ERR=200)
C
        ACTIVE = .TRUE.
        DO WHILE ( ACTIVE )
          READ(UNIT=LUNINP,FMT='(A)',END=100) RECORD
          CALL WORD(RECORD,I,J,K)
          IF ( K .GT. 0 ) THEN
            IF ( RECORD(I:I) .NE. '!' ) THEN
              IF ( NVMSUTIL .LT. MAXUTIL ) THEN
                NVMSUTIL = NVMSUTIL + 1
                VMSUTIL(NVMSUTIL) = RECORD(I:J)
              ELSE
                ACTIVE = .FALSE.
              ENDIF
            ENDIF
          ENDIF
        ENDDO
C
  100   CONTINUE
        CLOSE(UNIT=LUNINP)
C
C ****  Sort list of names to allow for binary search
C
        IF ( NVMSUTIL .GT. 0 ) THEN
          CALL SRTCHR(VMSUTIL,NVMSUTIL,IMAP)
        ENDIF
        GOTO 300
C
C ****  Cannot open file
C
  200   CONTINUE
      ENDIF
C
  300 CONTINUE
      OK = .TRUE.
      ROUTINE = STRING(1:1) .EQ. 'R'
C
C ****  Exclude internal system names
C
      IF     ( STRING(1:6) .EQ. 'COMMON' ) THEN

        SYSTEM(1) = '$CODE'
        SYSTEM(2) = '$PDATA'
        SYSTEM(3) = '$LOCAL'
        NN = 3
        CALL EXCLUDE_STRINGS (SYSTEM,NN,WORK,NAME,NUMBER)

      ELSEIF ( ROUTINE ) THEN
C
C ****  Create a list of routines of the form xxx$yyyyy
C
        NN = 0
        DO II =  1,NUMBER
          IF ( NN .LT. MAXSYS ) THEN
            IF ( NAME(II)(4:4) .EQ. '$' ) THEN
              NN = NN + 1
              SYSTEM(NN) = NAME(II)
              CALL WORD (SYSTEM(NN),I,J,WORK(NN))       ! Length of string
            ENDIF
          ENDIF
        ENDDO
C
C ****  Scan source file for xxx$yyyyy routines and exclude from
C ****  the list SYSTEM those routines called by the user.
C
        IF ( NN .GT. 0 ) THEN
          CALL D0C_EXCLUDE_STRINGS (SYSTEM,WORK,NN)
        ENDIF
C
C ****  Now exclude the remaining names in list SYSTEM from
C ****  the list NAME. The list NAME will then contain only those
C ****  routines which are called by the user.
C
        IF ( NN .GT. 0 ) THEN
          CALL EXCLUDE_STRINGS (SYSTEM,NN,WORK,NAME,NUMBER)
        ENDIF
      ENDIF

      IF ( NUMBER .LE. 0 ) GOTO 999

      LL = LEN (STRING)
      DO II =  1,NUMBER
        CALL WORD(NAME(II),I,J,N)
C
C ****  Check for invalid strings
C
        DO JJ =  1, MAXSYM

          IF ( INDEX (NAME(II),SYMBOL(JJ)) .GT. 0 ) THEN
C
C ****  VMS utility - check if this is a valid utility
C
            IF ( ROUTINE ) THEN
              CALL LOCSTR(NAME(II),VMSUTIL,NVMSUTIL,FOUND,ID)
C
              IF ( .NOT. FOUND ) THEN
                WRITE(UNIT=LUNOUT,FMT='(A,/,A,/'' '')')
     &  '%D0CHECK-W-BADUTIL, The following VMS utility is NOT allowed',
     &  '           ['//NAME(II)(I:J)//']'
                OK = .FALSE.
              ENDIF
C
            ELSE
C
C ****  Variables and Common Blocks
C
              WRITE(UNIT=LUNOUT,FMT='(A,/,A,/'' '')')
     &  '%D0CHECK-W-BADCHAR, The following '//STRING(1:LL)//
     &  ' name contains a ['//SYMBOL(JJ)//'] character',
     &  '           ['//NAME(II)(I:J)//']'
C
              OK = .FALSE.
            ENDIF
          ENDIF
        ENDDO
C
C ****  Check length of name
C
        IF ( N .GT. 31 ) THEN
          OK = .FALSE.
          WRITE(UNIT=LUNOUT,FMT='(A,/,'' '')')
     &        '%D0CHECK-W-BADLEN, Name: '//NAME(II)(1:N)//
     &        ' is too long'
        ENDIF

      ENDDO

  999 RETURN
      END
