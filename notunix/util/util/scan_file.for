      SUBROUTINE SCAN_FILE (PATTERN,OUTSTR,LINE,STATUS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Scan a text file and return the next line
C-   containing the string pattern given in variable PATTERN and the
C-   line number of that line within the file. Use SCAN_FILE_OPEN to
C-   open the file to be scanned. Wildcard characters can be used in
C-   the pattern string. The file is REWOUND when end-of-file is reached.
C-   Use SCAN_FILE_CLOSE to close file explicitly. Use SCAN_FILE_DELETE
C-   to delete the file.
C-
C-   Inputs  : PATTERN  [C*]    Pattern string
C-
C-   Outputs : OUTSTR   [C*]    File record matching pattern string
C-             LINE     [I]     Line number
C-             STATUS   [I]      0 ....OK
C-                              -1 ... End-of-file
C-                              -2 ... File not opened
C-
C-   Controls: None
C-
C-   Created   7-NOV-1989   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

      CHARACTER*(*) PATTERN
      CHARACTER*(*) OUTSTR
      INTEGER       LINE
      INTEGER       STATUS

      INTEGER LENGTH
      LOGICAL ACTIVE
      CHARACTER*132 RECORD,SUBSTR
      LOGICAL DEBUG
      DATA DEBUG /.FALSE./

      INCLUDE 'D0$UTIL$UTIL:SCAN_FILE_COM.INC'
C
      INTEGER I,J,K,II,JJ,LL
      LOGICAL MATCH_WILD
C----------------------------------------------------------------------
      OUTSTR = ' '
      LINE   = 0
      STATUS = 0
      CALL SWORDS (PATTERN,I,JJ,J)
C
C ****  Loop over file until a match has been found
C
      IF ( FILE_OPENED ) THEN
        ACTIVE = .TRUE.
        DO WHILE ( ACTIVE )
          READ (LUNIN,FMT='(A)',END=900) RECORD
          CALL SWORDS (RECORD,I,LL,J)
          LINE_NUMBER = LINE_NUMBER + 1
C
          IF ( MATCH_WILD (RECORD(1:LL),PATTERN(1:JJ)) ) THEN
            OUTSTR = RECORD
            LINE   = LINE_NUMBER
            ACTIVE = .FALSE.
          ENDIF
C
        ENDDO
        GOTO 999
      ELSE
        STATUS=-2
        GOTO 999
      ENDIF
C
  900 CONTINUE
      REWIND LUNIN                      ! Rewind file
      STATUS=-1
  999 RETURN
      END

      SUBROUTINE SCAN_FILE_OPEN (LUN,FILENAME,STATUS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Initialize SCAN_FILE: open the file to be
C-   scanned or if it is already open close and re-open it.
C-
C-   Inputs  : LUN      [I]     Input unit number.
C-             FILENAME [C*]    File-name to file to be scanned.
C-   Outputs : STATUS   [I]      0 ....OK
C-                              -3 ... Problem opening file
C-   Controls: None
C-
C-   Created   7-NOV-1989   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE

      INTEGER       LUN
      CHARACTER*(*) FILENAME
      INTEGER       STATUS

      INCLUDE 'D0$UTIL$UTIL:SCAN_FILE_COM.INC'

C----------------------------------------------------------------------
      LUNIN  = LUN
      SCAN_FILENAME = FILENAME
      STATUS = 0
      LINE_NUMBER = 0

      CALL SCAN_FILE_CLOSE
      OPEN (UNIT=LUNIN,FILE=FILENAME,STATUS='OLD',READONLY,ERR=900)
      FILE_OPENED = .TRUE.
      GOTO 999

  900 CONTINUE
      STATUS =-3
      FILE_OPENED = .FALSE.
  999 RETURN
      END

      SUBROUTINE SCAN_FILE_CLOSE
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
      INCLUDE 'D0$UTIL$UTIL:SCAN_FILE_COM.INC'
C----------------------------------------------------------------------
      INQUIRE (UNIT=LUNIN,OPENED=FILE_OPENED)
      IF ( FILE_OPENED ) THEN
        CLOSE(UNIT=LUNIN)
      ENDIF
  999 RETURN
      END

      SUBROUTINE SCAN_FILE_DELETE
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
      INCLUDE 'D0$UTIL$UTIL:SCAN_FILE_COM.INC'
C----------------------------------------------------------------------
      INQUIRE (UNIT=LUNIN,OPENED=FILE_OPENED)
      IF ( .NOT. FILE_OPENED ) THEN
        OPEN (UNIT=LUNIN,FILE=SCAN_FILENAME,STATUS='OLD')
      ENDIF
      CLOSE(UNIT=LUNIN,DISP='DELETE')
  999 RETURN
      END
