      SUBROUTINE GET_INCLUDES
     &  (LUN,ELEMENT,LIBRARY,LISMAX,LIST,NLIST,
     &  MODULE_NAME,MODULE_TYPE,ERROR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Scan a file or a CMS element for included files
C-   and return a list of includes. If LUN = 0 then scan the specified
C-   element in CMS. Note: the file or element is opened and closed
C-   internally.
C-
C-
C-   Inputs  : LUN      [I]     Input unit number if > 0
C-             ELEMENT  [C*]    File if LUN > 0; CMS element if LUN = 0
C-             LIBRARY  [C*]    CMS library LUN = 0
C-             LISMAX   [I]     Maximum size of LIST
C-
C-   Outputs : LIST(*)  [C*]    List of INCLUDES
C-             NLIST    [I]     Number of includes
C-             MODULE_NAME      [C*]    Name of module
C-             MODULE_TYPE      [C*]    Module type
C-                                              PROGRAM
C-                                              SUBROUTINE
C-                                              FUNCTION
C-                                              MODULE
C-             ERROR    [I]     0 ... OK
C-                             -1 ... Error opening file
C-   Controls: None
C-
C-   Notes:
C-
C-      Files with the following file-types can be processed:
C-
C-      .FOR .PAS .RNO
C-
C-   Created  24-AUG-1989   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

      INTEGER       LUN
      CHARACTER*(*) ELEMENT
      CHARACTER*(*) LIBRARY
      INTEGER       LISMAX
      CHARACTER*(*) LIST(*)
      INTEGER       NLIST
      CHARACTER*(*) MODULE_NAME
      CHARACTER*(*) MODULE_TYPE
      INTEGER       ERROR

      INTEGER I,J,K,L,N

      INTEGER FETCH_DATA_BLOCK(5)
      INTEGER STATUS
      INTEGER CMS$FETCH_GET
      INTEGER CMS$FETCH_OPEN
      INTEGER CMS$FETCH_CLOSE
      EXTERNAL CMS$_EOF

      INTEGER NOHISTORY
      PARAMETER( NOHISTORY = 1 )        ! Do not append history

      INTEGER       LENG
      CHARACTER*80 LINE,STRING
      LOGICAL FOUND_AN_INCLUDE,CONTINUE_SCAN

C----------------------------------------------------------------------

      NLIST = 0
      N     = LEN(LIBRARY)
      L     = LEN(ELEMENT)
      CALL WORD (LIBRARY(1:N),I,J,N)
      CALL WORD (ELEMENT(1:L),I,J,L)
C
C ****  Open file or CMS element
C
      IF ( LUN .GT. 0 ) THEN
        ERROR =-1
        OPEN (UNIT=LUN,FILE=ELEMENT,STATUS='OLD',READONLY,ERR=999)
      ELSE

        STATUS = CMS$FETCH_OPEN
     &          (       FETCH_DATA_BLOCK,
     &                  LIBRARY(1:N),
     &                  ELEMENT(1:L),
     &                  ,
     &                  NOHISTORY)
        IF ( .NOT. STATUS ) THEN
          ERROR =-1
          GOTO 999
        ENDIF
      ENDIF

      ERROR = 0                         ! Clear error code
      I = J - 3
C
C ****  Scan element according to type
C
      IF     ( ELEMENT(I:J) .EQ. '.FOR' ) THEN

        CALL INC_FORTRAN (LUN,
     &                    FETCH_DATA_BLOCK,
     &                    LISMAX,
     &                    LIST,
     &                    NLIST,
     &                    MODULE_NAME,
     &                    MODULE_TYPE,
     &                    ERROR)

      ELSEIF ( ELEMENT(I:J) .EQ. '.PAS' ) THEN

        CALL INC_PASCAL  (LUN,
     &                    FETCH_DATA_BLOCK,
     &                    LISMAX,
     &                    LIST,
     &                    NLIST,
     &                    MODULE_NAME,
     &                    MODULE_TYPE,
     &                    ERROR)

      ELSEIF ( ELEMENT(I:J) .EQ. '.RNO' ) THEN

        CALL INC_RUNOFF  (LUN,
     &                    FETCH_DATA_BLOCK,
     &                    LISMAX,
     &                    LIST,
     &                    NLIST,
     &                    MODULE_NAME,
     &                    MODULE_TYPE,
     &                    ERROR)

      ELSEIF ( ELEMENT(J-1:J) .EQ. '.C' ) THEN

        CALL INC_C       (LUN,
     &                    FETCH_DATA_BLOCK,
     &                    LISMAX,
     &                    LIST,
     &                    NLIST,
     &                    MODULE_NAME,
     &                    MODULE_TYPE,
     &                    ERROR)
      ENDIF
C
C ****  Close file
C
      IF ( LUN .GT. 0 ) THEN
        CLOSE(UNIT=LUN)
      ELSE
        STATUS = CMS$FETCH_CLOSE (FETCH_DATA_BLOCK)
      ENDIF
C
C ****  Strip off /LIST from includes
C
      IF ( NLIST .GT. 0 ) THEN
        DO J =  1,NLIST
          I = INDEX(LIST(J),'/')
          IF ( I .GT. 0 ) THEN
            LIST(J) = LIST(J)(1:I-1)
          ENDIF
        ENDDO
      ENDIF

  999 RETURN
      END

C---------------------------------------------------------------------
C---------------------------------------------------------------------

      SUBROUTINE INC_FORTRAN
     &  (LUN,FDB,LISMAX,LIST,NLIST,MODULE,TYPE,ERROR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Scan FORTRAN Source code for INCLUDEs.
C-   File is opened and closed EXTERNALLY.
C-
C-   Inputs  : LUN      [I]     Input unit number if > 0
C-             FDB(5)   [I]     Fetch Data Block
C-             LISMAX   [I]     Maximum size of LIST
C-
C-   Outputs : LIST(*)  [C*]    List of INCLUDES
C-             NLIST    [I]     Number of includes
C-             MODULE   [C*]    Module name
C-             TYPE     [C*]    Module type
C-                                 PROGRAM
C-                                 SUBROUTINE
C-                                 FUNCTION
C-             ERROR    [I]     status code
C-
C-   Controls: None
C-
C-   Created  31-AUG-1989   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

      INTEGER       LUN
      INTEGER       FDB(*)
      INTEGER       LISMAX
      CHARACTER*(*) LIST(*)
      INTEGER       NLIST
      CHARACTER*(*) MODULE
      CHARACTER*(*) TYPE
      INTEGER       ERROR

      INTEGER I,J,K,L,N

      INTEGER STATUS
      INTEGER CMS$FETCH_GET
      EXTERNAL CMS$_EOF

      INTEGER       LENG
      CHARACTER*80 LINE,STRING
      LOGICAL FOUND_AN_INCLUDE,CONTINUE_SCAN,GET_MODULE_NAME
C----------------------------------------------------------------------
C
C ****  Loop over lines in file
C
      NLIST = 0
      FOUND_AN_INCLUDE  = .FALSE.
      CONTINUE_SCAN     = .TRUE.
      GET_MODULE_NAME   = .TRUE.

      DO WHILE ( CONTINUE_SCAN )
C
C ****  Extract line from element; check for E-O-F
C
        IF ( LUN .GT. 0 ) THEN
          READ (UNIT=LUN,FMT='(A)',ERR=50,END=50) STRING
        ELSE
          STATUS = CMS$FETCH_GET (FDB,STRING)
          IF ( .NOT. STATUS ) GOTO 50
        ENDIF
C
C ****  Convert to upper case
C
        CALL UPCASE (STRING,STRING)
C
C ****  Extract module name and type
C
        IF ( GET_MODULE_NAME ) THEN
          IF ((STRING(1:1).EQ.' ').OR.(STRING(1:1).EQ.CHAR(9))) THEN
            I = INDEX(STRING,'PROGRAM')
            IF ( I .LE. 0 ) THEN
              I = INDEX(STRING,'SUBROUTINE')
            ENDIF
            IF ( I .LE. 0 ) THEN
              I = INDEX(STRING,'FUNCTION')
            ENDIF
C
            IF ( I .GT. 0 ) THEN
              GET_MODULE_NAME = .FALSE.   ! Found module name
              CALL WORD (STRING,I,J,K)
              TYPE = STRING(I:J)          ! Get type
C              STRING = STRING(J+1:)      ! Not nescessary
              CALL WORD (STRING(J+1:),I,J,K)
              MODULE = STRING(I:J)        ! Get module name
            ENDIF
          ENDIF
        ENDIF
C
C ****  Skip if end of includes reached; look for first non-comment
C ****  line containing an '=' sign.
C
        IF ( FOUND_AN_INCLUDE ) THEN
          IF ((STRING(1:1).EQ.' ').OR.(STRING(1:1).EQ.CHAR(9))) THEN
            IF ( INDEX(STRING,'=') .GT. 0 ) THEN
              CONTINUE_SCAN = .FALSE.
            ENDIF
          ENDIF
        ENDIF

        IF ( CONTINUE_SCAN ) THEN
          IF ((STRING(1:1).EQ.' ').OR.(STRING(1:1).EQ.CHAR(9))) THEN
            I = INDEX(STRING,'INCLUDE ')
            IF ( I .GT. 0 ) THEN

              CALL WORD (STRING(I+LEN('INCLUDE '):),K,J,N)     ! Extract include
              K = K + I + LEN('INCLUDE ')
              J = J + I + LEN('INCLUDE ') - 2
              LENG  = N - 2
              LINE  = STRING(K:J)
C
C ****  Accept includes which begin with D0$
C
              IF ( LINE(1:3) .EQ. 'D0$' ) THEN
                FOUND_AN_INCLUDE = .TRUE.

                IF ( NLIST .LT. LISMAX ) THEN
                  NLIST = NLIST + 1
                  LIST(NLIST) = LINE(1:LENG)
                ELSE
                  GOTO 50
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDDO

   50 CONTINUE
  999 RETURN
      END

      SUBROUTINE INC_PASCAL
     &  (LUN,FDB,LISMAX,LIST,NLIST,MODULE,TYPE,ERROR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Scan PASCAL Source code for PEN files
C-   and INCLUDEs.
C-   File is opened and closed externally.
C-
C-   Inputs  : LUN      [I]     Input unit number if > 0
C-             FDB(5)   [I]     Fetch Data Block
C-             LISMAX   [I]     Maximum size of LIST
C-
C-   Outputs : LIST(*)  [C*]    List of INCLUDES
C-             NLIST    [I]     Number of includes
C-             MODULE   [C*]    Module name
C-             TYPE     [C*]    Module type
C-                                 PROGRAM
C-                                 MODULE
C-             ERROR    [I]     status code
C-
C-   Controls: None
C-
C-   Created  31-AUG-1989   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

      INTEGER       LUN
      INTEGER       FDB(*)
      INTEGER       LISMAX
      CHARACTER*(*) LIST(*)
      INTEGER       NLIST
      CHARACTER*(*) MODULE
      CHARACTER*(*) TYPE
      INTEGER       ERROR

      INTEGER I,J,K,L,N,TOKEN_POSN,TOKEN_LINE

      INTEGER MAX_LINES,LINES
      PARAMETER( MAX_LINES = 255 )
      INTEGER      TEXT_LENGTH(MAX_LINES)
      CHARACTER*80 TEXT_BUFFER(MAX_LINES)

      INTEGER STATUS
      INTEGER CMS$FETCH_GET
      EXTERNAL CMS$_EOF

      INTEGER ISTART,IEND,II,JJ,KK,NN,LAST_LINE
      CHARACTER*80 RECORD,STRING
      LOGICAL CONTINUE_SCAN,SKIP,GET_MODULE_NAME
C----------------------------------------------------------------------
C
C ****  Loop over lines in file
C
C
C ****  Extract INHERIT attributes list if one is present
C
      CALL EXTRACT_ATTRIBUTE_LIST (
     &          LUN,
     &          FDB,
     &          'INHERIT',
     &          TOKEN_POSN,
     &          TOKEN_LINE,
     &          TEXT_BUFFER,
     &          TEXT_LENGTH,
     &          LINES,
     &          MAX_LINES)

      LAST_LINE = LINES                 ! Note end of attribute list
      CONTINUE_SCAN     = .TRUE.
C
C ****  Extract %INCLUDES
C
      SKIP = .TRUE.
C
      DO WHILE ( CONTINUE_SCAN )
C
C ****  Extract line from element; check for E-O-F
C
        IF ( SKIP ) THEN
          SKIP = .FALSE.
          CALL EXTRACT_LAST_RECORD (RECORD)
        ELSE
          IF ( LUN .GT. 0 ) THEN
            READ (UNIT=LUN,FMT='(A)',ERR=50,END=50) RECORD
          ELSE
            STATUS = CMS$FETCH_GET (FDB,RECORD)
            IF ( .NOT. STATUS ) GOTO 50
          ENDIF
        ENDIF
C
        CALL SWORDS (RECORD,ISTART,IEND,NN)
        CALL UPCASE (RECORD(1:IEND),STRING(1:IEND))
C
C ****  Extract module name and type
C
        IF ( GET_MODULE_NAME ) THEN
          I = INDEX(STRING(1:8),'PROGRAM ')
          IF ( I .LE. 0 ) THEN
            I = INDEX(STRING(1:8),'MODULE ')
          ENDIF
C
          IF ( I .GT. 0 ) THEN
            GET_MODULE_NAME = .FALSE.   ! Found module name
            CALL WORD (STRING,I,J,K)
            TYPE = STRING(I:J)          ! Get type
            STRING = STRING(J+1:)
            CALL WORD (STRING,I,J,K)
            IF ( STRING(J:J) .EQ. ';' ) THEN
              J = J - 1               ! Remove semi-colon
            ENDIF
            MODULE = STRING(I:J)        ! Get module name
          ENDIF
        ENDIF

        I = INDEX(STRING(1:IEND),'%INCLUDE')
        IF ( I .GT. 0 ) THEN
          LINES = LINES + 1
          TEXT_BUFFER(LINES) = RECORD(1:IEND)
          TEXT_LENGTH(LINES) = IEND
        ENDIF

        I = INDEX(STRING(1:IEND),'%DICTIONARY')
        IF ( I .GT. 0 ) THEN
          LINES = LINES + 1
          TEXT_BUFFER(LINES) = RECORD(1:IEND)
          TEXT_LENGTH(LINES) = IEND
        ENDIF

      ENDDO

   50 CONTINUE
C
C ****  Extract all strings bounded by single quotes and which
C ****  do not begin with SYS$
C
C
C ****  Extract PEN files
C
      NLIST = 0
      IF ( LAST_LINE .GT. 0 ) THEN

        DO I =  1, LAST_LINE
          II   = 1
          IEND = TEXT_LENGTH(I)
C
C ****  Convert to upper case
C
          CALL UPCASE (TEXT_BUFFER(I)(II:IEND),TEXT_BUFFER(I)(II:IEND))

   60     CONTINUE

          KK = INDEX(TEXT_BUFFER(I)(II:IEND),'''')
          IF ( KK .GT. 0 ) THEN
            II = KK + II - 1
            JJ = INDEX(TEXT_BUFFER(I)(II+1:IEND),'''') + II

            IF ( TEXT_BUFFER(I)(II+1:II+4) .NE. 'SYS$' ) THEN
              NLIST = NLIST + 1

              IF ( INDEX(TEXT_BUFFER(I),'.PEN') .EQ. 0 ) THEN
                LIST(NLIST) = TEXT_BUFFER(I)(II+1:JJ-1)//'.PEN'
              ELSE
                LIST(NLIST) = TEXT_BUFFER(I)(II+1:JJ-1)
              ENDIF

            ENDIF

            II = JJ + 1
            GOTO 60
          ENDIF
        ENDDO
      ENDIF
C
C ****  Extract INCLUDES
C
      IF ( LINES .GT. 0 ) THEN

        DO I =  LAST_LINE+1, LINES
          IEND = TEXT_LENGTH(I)
          II = INDEX(TEXT_BUFFER(I)(1:IEND),'''')
          IF ( II .GT. 0 ) THEN
            JJ = INDEX(TEXT_BUFFER(I)(II+1:IEND),'''') + II
            NLIST = NLIST + 1
            LIST(NLIST) = TEXT_BUFFER(I)(II+1:JJ-1)
            JJ = INDEX(LIST(NLIST),'/')
            IF ( JJ .GT. 0 ) THEN
              LIST(NLIST) = LIST(NLIST)(1:JJ-1)
            ENDIF
          ENDIF
        ENDDO

      ENDIF

  999 RETURN
      END

      SUBROUTINE INC_RUNOFF
     &  (LUN,FDB,LISMAX,LIST,NLIST,MODULE,TYPE,ERROR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Scan RUNOFF Source code for .REQUIRE
C-   statements.
C-   File is opened and closed externally.
C-
C-   Inputs  : LUN      [I]     Input unit number if > 0
C-             FDB(5)   [I]     Fetch Data Block
C-             LISMAX   [I]     Maximum size of LIST
C-
C-   Outputs : LIST(*)  [C*]    List of INCLUDES
C-             NLIST    [I]     Number of includes
C-             MODULE   [C*]    Module name
C-             TYPE     [C*]    Module type
C-                                 PROGRAM
C-                                 SUBROUTINE
C-                                 FUNCTION
C-             ERROR    [I]     status code
C-
C-   Controls: None
C-
C-   Created  31-AUG-1989   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

      INTEGER       LUN
      INTEGER       FDB(*)
      INTEGER       LISMAX
      CHARACTER*(*) LIST(*)
      INTEGER       NLIST
      CHARACTER*(*) MODULE
      CHARACTER*(*) TYPE
      INTEGER       ERROR

      INTEGER I,J,K,L,N

      INTEGER STATUS
      INTEGER CMS$FETCH_GET
      EXTERNAL CMS$_EOF

      INTEGER       LENG
      CHARACTER*80 LINE,STRING
      LOGICAL CONTINUE_SCAN
C----------------------------------------------------------------------
C
C ****  Loop over lines in file
C
      NLIST = 0
      CONTINUE_SCAN = .TRUE.
      MODULE = ' '
      TYPE   = ' '
      DO WHILE ( CONTINUE_SCAN )
C
C ****  Extract line from element; check for E-O-F
C
        IF ( LUN .GT. 0 ) THEN
          READ (UNIT=LUN,FMT='(A)',ERR=50,END=50) STRING
        ELSE
          STATUS = CMS$FETCH_GET (FDB,STRING)
          IF ( .NOT. STATUS ) GOTO 50
        ENDIF
C
        IF ( STRING(1:4) .EQ. '.REQ' ) THEN
          I = INDEX (STRING,'"')
          IF ( I .GT. 0 ) THEN
            J = INDEX (STRING(I+1:),'"')
            IF ( J .GT. 0 ) THEN
              I = I + 1             ! Determine start and end
              J = I + J - 2         ! of sub-string
              IF ( NLIST .LT. LISMAX ) THEN
                NLIST = NLIST + 1
                LIST(NLIST) = STRING(I:J)
              ELSE
                CONTINUE_SCAN = .FALSE.
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDDO

   50 CONTINUE
  999 RETURN
      END

      SUBROUTINE INC_C
     &  (LUN,FDB,LISMAX,LIST,NLIST,MODULE,TYPE,ERROR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Scan C Source code for INCLUDE
C-   statements.
C-   File is opened and closed externally.
C-
C-   Inputs  : LUN      [I]     Input unit number if > 0
C-             FDB(5)   [I]     Fetch Data Block
C-             LISMAX   [I]     Maximum size of LIST
C-
C-   Outputs : LIST(*)  [C*]    List of INCLUDES
C-             NLIST    [I]     Number of includes
C-             MODULE   [C*]    Module name
C-             TYPE     [C*]    Module type
C-                                 PROGRAM
C-                                 SUBROUTINE
C-                                 FUNCTION
C-             ERROR    [I]     status code
C-
C-   Controls: None
C-
C-   Created  31-AUG-1989   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

      INTEGER       LUN
      INTEGER       FDB(*)
      INTEGER       LISMAX
      CHARACTER*(*) LIST(*)
      INTEGER       NLIST
      CHARACTER*(*) MODULE
      CHARACTER*(*) TYPE
      INTEGER       ERROR

      INTEGER I,J,K,L,N

      INTEGER STATUS
      INTEGER CMS$FETCH_GET
      EXTERNAL CMS$_EOF

      INTEGER       LENG
      CHARACTER*80 LINE,STRING
      LOGICAL CONTINUE_SCAN
C----------------------------------------------------------------------
C
C ****  Loop over lines in file
C
      NLIST = 0
      CONTINUE_SCAN     = .TRUE.

      DO WHILE ( CONTINUE_SCAN )
C
C ****  Extract line from element; check for E-O-F
C
        IF ( LUN .GT. 0 ) THEN
          READ (UNIT=LUN,FMT='(A)',ERR=50,END=50) STRING
        ELSE
          STATUS = CMS$FETCH_GET (FDB,STRING)
          IF ( .NOT. STATUS ) GOTO 50
        ENDIF

      ENDDO

   50 CONTINUE
  999 RETURN
      END
