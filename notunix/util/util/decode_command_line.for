      SUBROUTINE DECODE_COMMAND_LINE
     &(MODULE,FACILITY,QUALIFIER,NQUAL,PRESENT,FILENAME,LENF,NFILE,
     &STATUS_CODE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Extract filenames and qualifiers from
C-                          a command line using the CLI syntax analysis
C-                          procedures.
C-
C-   Inputs  : MODULE           CLD module name (declared EXTERNAL)
C-             FACILITY   [C*]  Command name
C-             QUALIFIER  [C*]  Array of global qualifiers without values
C-             NQUAL            Number of elements in array
C-
C-   Outputs : PRESENT    [L*]  Array of logicals. TRUE if qualifier present,
C-                              FALSE otherwise
C-             FILNAME    [C*]  Array of file-names
C-             LENF       [I*]  Array of file-name lengths
C-             NFILE            Number of elements in array
C-             STATUS_CODE      Return code
C-   Controls:
C-
C-   Created  14-MAY-1989   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

      EXTERNAL      MODULE
      CHARACTER*(*) FACILITY
      CHARACTER*(*) FILENAME(*)
      INTEGER       LENF(*)
      INTEGER       NFILE
      CHARACTER*(*) QUALIFIER(*)
      LOGICAL       PRESENT(*)
      INTEGER       NQUAL
      INTEGER       STATUS_CODE

      INTEGER I,J,K,L,N
      CHARACTER*4095 COMMAND
      INTEGER LENGTH
      INTEGER STATUS
C
C ****  Define run-time library entry points
C
      INTEGER  LIB$GET_FOREIGN

C----------------------------------------------------------------------
C
C ****  Get command line
C
      STATUS = LIB$GET_FOREIGN(COMMAND,,LENGTH)
      IF ( .NOT. STATUS ) CALL LIB$SIGNAL (%VAL(STATUS))
C
C ****  Parse command line
C
      L = LEN(FACILITY)
      COMMAND = FACILITY(1:L)//' '//COMMAND(1:LENGTH)

      CALL DECODE_COMMAND (MODULE,
     &                     'FILE_SPEC',
     &                     COMMAND(1:LENGTH+L+1),
     &                     QUALIFIER,
     &                     NQUAL,
     &                     PRESENT,
     &                     FILENAME,
     &                     LENF,
     &                     NFILE,
     &                     STATUS_CODE)

  999 RETURN
      END

      SUBROUTINE DECODE_COMMAND
     &(MODULE,TYPE,COMMAND,QUALIFIER,NQUAL,PRESENT,FILENAME,
     &LENF,NFILE,STATUS_CODE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Extract filenames and global qualifiers from
C-                          a command string.
C-
C-   Inputs  : MODULE           CLD module name (declared EXTERNAL)
C-             TYPE       [C*]  Type: FILE_SPEC, ELEMENT etc.
C-             COMMAND    [C*]  Command line
C-             QUALIFIER  [C*]  Array of global qualifiers without values
C-             NQUAL            Number of elements in array
C-
C-   Outputs : PRESENT    [L*]  Array of logicals. TRUE if qualifier present,
C-                              FALSE otherwise
C-             FILNAME    [C*]  Array of file-names
C-             LENF       [I*]  Array of file-name lengths
C-             NFILE            Number of elements in array
C-             STATUS_CODE [I]  Return status
C-   Controls:
C-
C-   Created  14-MAY-1989   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

      EXTERNAL      MODULE
      CHARACTER*(*) TYPE
      CHARACTER*(*) COMMAND
      CHARACTER*(*) FILENAME(*)
      INTEGER       LENF(*)
      INTEGER       NFILE
      CHARACTER*(*) QUALIFIER(*)
      LOGICAL       PRESENT(*)
      INTEGER       NQUAL
      INTEGER       STATUS_CODE

C----------------------------------------------------------------------
      CALL DECODE_PARSE (MODULE,COMMAND,STATUS_CODE)
      IF ( STATUS_CODE .NE. 0 ) GOTO 999
C
      CALL DECODE_QUALIFIERS (QUALIFIER,NQUAL,PRESENT)
      CALL DECODE_FILENAMES  (TYPE,FILENAME,LENF,NFILE,STATUS_CODE)
  999 RETURN
      END

      SUBROUTINE DECODE_PARSE (MODULE,COMMAND,STATUS_CODE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Parse a command string.
C-
C-   Inputs  : MODULE           CLD module name (declared EXTERNAL)
C-             COMMAND    [C*]  Command line
C-
C-   Outputs : STATUS_CODE [I]  Return status
C-
C-   Controls:
C-
C-   Created  14-MAY-1989   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

      EXTERNAL      MODULE
      CHARACTER*(*) COMMAND
      INTEGER       STATUS_CODE

      INTEGER I,J,K,L,N
      INTEGER LENGTH
      INTEGER STATUS
C
C ****  Define LIB and CLI entry points and symbols
C
      INTEGER CLI$DCL_PARSE
      INTEGER LIB$GET_INPUT
      EXTERNAL LIB$GET_INPUT
C----------------------------------------------------------------------
C
C ****  Get length of command line
C
      CALL SWORDS (COMMAND,I,LENGTH,J)
C
C ****  Parse command line
C
      STATUS = CLI$DCL_PARSE(COMMAND(1:LENGTH),
     &                       MODULE,
     &                       LIB$GET_INPUT)

      IF ( .NOT. STATUS ) THEN
        STATUS_CODE =-99
      ELSE
        STATUS_CODE = 0
      ENDIF

  999 RETURN
      END


      SUBROUTINE DECODE_QUALIFIERS (QUALIFIER,NQUAL,PRESENT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  For given qualifier return TRUE if the
C-   qualifier is present.
C-
C-   Inputs  : QUALIFIER  [C*]  Array of global qualifiers without values
C-             NQUAL            Number of elements in array
C-
C-   Outputs : PRESENT    [L*]  Array of logicals. TRUE if qualifier present,
C-                              FALSE otherwise
C-   Controls:
C-
C-   Created  14-MAY-1989   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

      CHARACTER*(*) QUALIFIER(*)
      INTEGER       NQUAL
      LOGICAL       PRESENT(*)

      INTEGER I,J,K,L,N
C
C ****  Define LIB and CLI entry points and symbols
C
      INTEGER CLI$PRESENT
      EXTERNAL CLI$_PRESENT
      EXTERNAL CLI$_ABSENT

C----------------------------------------------------------------------

C *********************************************
C ****  Determine which qualifiers are present
C *********************************************

      IF ( NQUAL .GT. 0 ) THEN
        DO I = 1, NQUAL
          CALL WORD (QUALIFIER(I),J,K,N)
          IF ( CLI$PRESENT ( QUALIFIER(I)(J:K) ) ) THEN
            PRESENT(I) = .TRUE.
          ELSE
            PRESENT(I) = .FALSE.
          ENDIF
        ENDDO
      ENDIF

  999 RETURN
      END

      SUBROUTINE DECODE_FILENAMES(TYPE,FILENAME,LENF,NFILE,STATUS_CODE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Extract filenames either from a parameter
C-                          or a qualifier. TYPE = 'FILE_SPEC' for a
C-                          parameter, TYPE = 'ELEMENT' for a qualifier.
C-                          Also, apply /EXCLUDE list if present, and
C-                          if TYPE = 'INCLUDE' apply /OMIT.
C-
C-   Inputs  : TYPE       [C*]  Type: FILE_SPEC, ELEMENT etc.
C-
C-   Outputs : FILNAME    [C*]  Array of file-names
C-             LENF       [I*]  Array of file-name lengths
C-             NFILE            Number of elements in array
C-             STATUS_CODE [I]  Return status
C-   Controls:
C-
C-   Created  14-MAY-1989   Harrison B. Prosper
C-   Updated  29-NOV-1989   Harrison B. Prosper
C-      Added INCLUDE type
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

      CHARACTER*(*) TYPE
      CHARACTER*(*) FILENAME(*)
      INTEGER       LENF(*)
      INTEGER       NFILE
      INTEGER       STATUS_CODE

      INTEGER NTOTAL,NLIST,I,J,K,L,N,II,JJ,KK
      INTEGER       LISMAX
      PARAMETER(    LISMAX = 1000 )
      INTEGER       LLEN(LISMAX),WORK(LISMAX)
      CHARACTER*132 LIST(LISMAX),TEMP(LISMAX)
      CHARACTER*132 STRING
      CHARACTER*32  EXCLUDE_QUALIFIER
      LOGICAL       EXCLUDE,FOUND
      INTEGER       LENGTH,LSTRING,LQUAL
      INTEGER STATUS
C
C ****  Define LIB and CLI entry points and symbols
C
      INTEGER CLI$PRESENT
      INTEGER CLI$GET_VALUE
      EXTERNAL CLI$_PRESENT
      EXTERNAL CLI$_ABSENT

      INCLUDE 'D0$UTIL$UTIL:DECODE_COMMAND_COM.INC'
C----------------------------------------------------------------------

C *********************************************
C ****  Get file-spec-list
C *********************************************
C
C
C ****  Note: If file-spec is defaulted the specs
C ****  will be separated by commas.
C
      NFILE  = 0
      CALL WORD (TYPE,I,J,L)
C
C ****  Check which exclusion qualifier to use.
C
      IF ( TYPE(I:J) .EQ. 'INCLUDE' ) THEN
        EXCLUDE_QUALIFIER = 'OMIT'
        LQUAL = 4
      ELSE
        EXCLUDE_QUALIFIER = 'EXCLUDE'
        LQUAL = 7
      ENDIF
      

      STATUS = CLI$GET_VALUE (TYPE(I:J), STRING, LSTRING)

      DO WHILE ( STATUS .AND. NFILE .LT. LISMAX )

        IF ( NFILE .LE. 0 ) THEN
          CALL PARSE (STRING,',',FILENAME(1),LENF(1),NFILE)
        ELSE
          NFILE = NFILE + 1
          FILENAME(NFILE) = STRING
          LENF(NFILE)     = LSTRING
        ENDIF

        STATUS = CLI$GET_VALUE (TYPE(I:J), STRING, LSTRING)

      ENDDO

      IF (STATUS .NE. %LOC (CLI$_ABSENT)) CALL LIB$SIGNAL (%VAL(STATUS))

      IF ( NFILE .GT. 0 ) THEN
        IF ( READ_FROM_FILE ) THEN
          CALL EXPAND_LIST_SETUP (LUNIN,SCANFILE,STATUS_CODE)
        ENDIF
        CALL EXPAND_LIST (TEMP,FILENAME,LENF,NFILE,STATUS_CODE,LISMAX)
      ENDIF

      IF ( NFILE .LE. 0 ) THEN
        STATUS_CODE = -1
        GOTO 999
      ENDIF

C *********************************************
C ****  Get exclusion file-spec-list
C *********************************************
C
      NLIST   = 0
      EXCLUDE = .FALSE.

      IF ( CLI$PRESENT (EXCLUDE_QUALIFIER(1:LQUAL)) ) THEN

        EXCLUDE = .TRUE.
        STATUS = CLI$GET_VALUE (EXCLUDE_QUALIFIER(1:LQUAL), 
     &                          STRING, 
     &                          LSTRING)

        DO WHILE ( STATUS .AND. NLIST .LT. LISMAX )

          NLIST       = NLIST + 1
          LIST(NLIST) = STRING
          LLEN(NLIST) = LSTRING

          STATUS = CLI$GET_VALUE (EXCLUDE_QUALIFIER(1:LQUAL), 
     &                            STRING, 
     &                            LSTRING)

        ENDDO

      ENDIF

      IF (STATUS .NE. %LOC (CLI$_ABSENT)) THEN
        CALL LIB$SIGNAL (%VAL(STATUS))
      ENDIF

      IF ( EXCLUDE ) THEN
        IF ( READ_FROM_FILE ) THEN
          CALL EXPAND_LIST_SETUP (LUNIN,SCANFILE,STATUS_CODE)
        ENDIF
        CALL EXPAND_LIST (TEMP,LIST,LLEN,NLIST,STATUS_CODE,LISMAX)
        EXCLUDE = NLIST .GT. 0
      ENDIF

C *********************************************
C ****  Apply exclusion list
C *********************************************

      IF ( EXCLUDE ) THEN

        CALL EXCLUDE_STRINGS (LIST,NLIST,WORK,FILENAME,NFILE)

        IF ( NFILE .GT. 0 ) THEN
          DO I =  1,NFILE
            CALL WORD (FILENAME(I),II,JJ,LENF(I))
          ENDDO

        ELSE
          STATUS_CODE = -2
        ENDIF

      ENDIF

  999 CONTINUE
      READ_FROM_FILE = .FALSE.
      RETURN
      END

      SUBROUTINE DECODE_COMMAND_SETUP (LUN,FILENAME)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   7-NOV-1989   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER       LUN
      CHARACTER*(*) FILENAME

      INCLUDE 'D0$UTIL$UTIL:DECODE_COMMAND_COM.INC'
C----------------------------------------------------------------------
      LUNIN    = LUN
      SCANFILE = FILENAME(1:LEN(FILENAME))
      READ_FROM_FILE = .TRUE.
  999 RETURN
      END

      SUBROUTINE GET_DEFAULT_DIRECTORY (DIRECTORY,L)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return name of default directory
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  15-APR-1989   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) DIRECTORY
      INTEGER CONTEXT,STATUS,L
      INTEGER LIB$FIND_FILE
      DATA CONTEXT /0/
C----------------------------------------------------------------------
C
      L = LEN (DIRECTORY)
      STATUS = LIB$FIND_FILE
     &            ('*.*',DIRECTORY(1:L),CONTEXT,,,,)
      IF ( .NOT. STATUS ) THEN
        L = 0
        DIRECTORY = ' '
      ELSE
        L = INDEX(DIRECTORY,']')
        DIRECTORY = DIRECTORY(:L)
      ENDIF
  999 RETURN
      END

      SUBROUTINE FIND_FILE (FILE_SPEC,FILE_LEN,FILE_NAME,N,FOUND)
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) FILE_SPEC
      CHARACTER*(*) FILE_NAME
      LOGICAL FOUND
      INTEGER I,J,N,L,CONTEXT,FILE_LEN,STATUS
      INTEGER LIB$FIND_FILE
      DATA CONTEXT /0/
C----------------------------------------------------------------------
C
C ****  Get name of next file to be processed
C
      L = LEN(FILE_NAME)
      STATUS = LIB$FIND_FILE
     &            (FILE_SPEC(1:FILE_LEN),FILE_NAME(1:L),CONTEXT,,,,)
      IF ( .NOT. STATUS ) THEN
        FOUND = .FALSE.
      ELSE
        FOUND = .TRUE.
        N = INDEX(FILE_NAME,';')-1
        FILE_NAME = FILE_NAME(1:N)
      ENDIF
  999 RETURN
      END

      SUBROUTINE EXPAND_LIST (TEMP,LIST,LLEN,NLIST,STATUS_CODE,LISMAX)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Given a list of file-specs LIST(*) possibly
C-   containing wildcard characters find the files that match the list
C-   of names and return an expanded list of names without wildcard
C-   characters. If a file should be scanned instead of directories
C-   you should supply the file name using EXPAND_LIST_SETUP.
C-
C-   Inputs  : TEMP(*)  [C*]    Working array with at least NLIST elements
C-
C-             LIST(*)  [C*]    List of file-specs
C-             LLEN(*)  [I]     Length of file-specs strings
C-             NLIST    [I]     Number of file-specs
C-
C-   Outputs : LIST(*)  [C*]    List of file-specs
C-             LLEN(*)  [I]     Length of file-specs strings
C-             NLIST    [I]     Number of file-specs
C-             STATUS_CODE [I]  0--- OK, -1 LISMAX limit reached.
C-
C-   Controls: LISMAX   [I]     Maximum number of file-specs
C-
C-   Created   7-NOV-1989   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

      CHARACTER*(*) LIST(*)
      INTEGER       LLEN(*)
      INTEGER       NLIST
      CHARACTER*(*) TEMP(*)
      INTEGER       LISMAX
      INTEGER       STATUS_CODE
C
      CHARACTER*132 NEXT_NAME
      LOGICAL FOUND,NOT_FOUND
      INTEGER LINE_NUMBER,CODE
      INTEGER IEX,KEX,CONTEXT,I,J,K,L,II,JJ,KK,STATUS
      INTEGER LIB$FIND_FILE
C
      INCLUDE 'D0$UTIL$UTIL:EXPAND_LIST_COM.INC'
C----------------------------------------------------------------------
      STATUS_CODE = 0
C
C ****  Construct ordered list of files
C
      DO I =  1,NLIST
C
C ****  Check for directory-spec and extension
C
        J = INDEX(LIST(I),']')
        IF ( J .EQ. LLEN(I) ) THEN
          TEMP(I) = LIST(I)
        ELSE

          J = INDEX(LIST(I)(J+1:LLEN(I)),'.')
          IF ( J .GT. 0 ) THEN
            TEMP(I) = LIST(I)
          ELSE
            J = INDEX (LIST(I),' ')
            IF ( J .GT. 0 ) THEN
              TEMP(I) = LIST(I)(1:J-1)//'.FOR'    ! Assume extension .FOR
            ELSE
              TEMP(I) = LIST(I)//'.FOR'
            ENDIF
          ENDIF
        ENDIF
C
C ****  Add directory prefix a la VMS.
C
        IF ( I .GT. 1 ) THEN
          J = INDEX (TEMP(I),']')       ! If current file-spec hasn't a
          IF ( J .LE. 0 ) THEN          ! directory prefix then check previous
            J = INDEX (TEMP(I-1),']')   ! file-spec.
            IF ( J .GT. 0 ) THEN
              TEMP(I) = TEMP(I-1)(1:J)//TEMP(I)
            ENDIF
          ENDIF
        ENDIF
      ENDDO
      KEX   = 0
C
      DO K =  1,NLIST
C
        CONTEXT = 0
        FOUND = .TRUE.
        DO WHILE ( FOUND )
C
C ****  Read either from a file or search directories.
C
          IF ( READ_FROM_FILE ) THEN
            CALL SCAN_FILE (TEMP(K),
     &                      NEXT_NAME,
     &                      LINE_NUMBER,
     &                      CODE)
C
            CALL EXTRACT_FILENAME(NEXT_NAME,II,JJ,KK)
            NEXT_NAME = NEXT_NAME(II:JJ)//';'
            NOT_FOUND = CODE .NE. 0
          ELSE
            STATUS = LIB$FIND_FILE (TEMP(K),NEXT_NAME,CONTEXT,,,,)
            NOT_FOUND = .NOT. STATUS
          ENDIF
C
          IF ( NOT_FOUND ) THEN
            FOUND = .FALSE.
            J = INDEX(TEMP(K),']')          ! Check for directory-spec
            IF ( J .EQ. LLEN(K) ) THEN
              IF ( KEX .LT. LISMAX ) THEN
                KEX = KEX + 1
                LLEN(KEX) = LLEN(K)
                LIST(KEX) = TEMP(K)
              ELSE
                STATUS_CODE =-1         ! Not enough space
              ENDIF
            ENDIF
          ELSE
            IF ( KEX .LT. LISMAX ) THEN
              KEX = KEX + 1
              LLEN(KEX) = INDEX(NEXT_NAME,';')-1
              LIST(KEX) = NEXT_NAME(1:LLEN(KEX))
            ELSE
              STATUS_CODE =-1
            ENDIF
          ENDIF
        ENDDO
      ENDDO
C
      NLIST = KEX
  999 CONTINUE
C
C ****  Close file being scanned
C
      IF ( READ_FROM_FILE ) THEN
        READ_FROM_FILE = .FALSE.
        CALL SCAN_FILE_CLOSE
      ENDIF
      RETURN
      END

      SUBROUTINE EXPAND_LIST_SETUP (LUN,FILENAME,STATUS_CODE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   7-NOV-1989   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER       LUN
      CHARACTER*(*) FILENAME
      INTEGER       STATUS_CODE
      INCLUDE 'D0$UTIL$UTIL:EXPAND_LIST_COM.INC'
C----------------------------------------------------------------------
      CALL SCAN_FILE_OPEN (LUN,FILENAME,STATUS_CODE)
      LUNSCAN        = LUN
      READ_FROM_FILE = .TRUE.
      SCANFILE       = FILENAME
  999 RETURN
      END

      SUBROUTINE GET_VALUES (QUALIFIER, VALUE, LENV, NVAL, LISMAX)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get values associated with specified
C-                         qualifiers.
C-
C-   Inputs  : QUALIFIER        [C*]    Name of Qualifier
C-
C-   Outputs : VALUE(*)         [C*]    Values (as strings)
C-             LENV(*)          [I]     Length of value strings
C-             NVAL             [I]     Number of values
C-
C-   Controls: LISMAX           [I]     Maximum number of values
C-                                      to return
C-
C-   Created  11-JUL-1989   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE


      CHARACTER*(*) QUALIFIER
      CHARACTER*(*) VALUE(*)
      INTEGER       LENV(*)
      INTEGER       NVAL
      INTEGER       LISMAX

      INTEGER I,J,K,L,LSTRING
      CHARACTER*132 STRING

      INTEGER STATUS
      INTEGER CLI$GET_VALUE
      INTEGER CLI$PRESENT
      EXTERNAL CLI$_ABSENT
C----------------------------------------------------------------------

      L = LEN (QUALIFIER)
      NVAL  = 0
      IF ( .NOT. CLI$PRESENT ( QUALIFIER(1:L) ) ) GOTO 999

      STATUS = CLI$GET_VALUE (QUALIFIER(1:L), STRING, LSTRING)

      DO WHILE ( STATUS .AND. NVAL .LT. LISMAX )

        NVAL = NVAL + 1
        VALUE(NVAL) = STRING
        LENV (NVAL) = LSTRING

        STATUS = CLI$GET_VALUE (QUALIFIER(1:L), STRING, LSTRING)

      ENDDO

      IF (STATUS .NE. %LOC (CLI$_ABSENT)) CALL LIB$SIGNAL
     &    (%VAL(STATUS))
      IF (NVAL .LE. 0 ) GOTO 999
C
C ****  Remove double quotes, if present
C
      DO K = 1, NVAL

        I = 1
        J = LENV(K)

        IF ( VALUE(K)(I:I) .EQ. '"' ) THEN
          I = I + 1
        ENDIF
        IF ( VALUE(K)(J:J) .EQ. '"' ) THEN
          J = J - 1
        ENDIF

        VALUE(K)= VALUE(K)(I:J)
        LENV(K) = J - I + 1

      ENDDO

  999 RETURN
      END
