      SUBROUTINE EZREAD (LUN,BKNAME,WRDREC,LSUPP,IZLINK)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-      Read a TEXT file containing parameters and save records and
C-      decoded values in an RCP bank. The records are ordered to allow
C-      the use of a binary search to retrieve data from the bank. The
C-      RCP bank is assigned the name given in BKNAME. The name can be
C-      changed with EZRNAM. The file should be opened and closed
C-      externally. Note: INRCP is easier to use.
C-
C-   Inputs  : LUN      [I]     Logical unit of input stream
C-             BKNAME   [C*]    Name to give to RCP bank
C-             WRDREC   [I]     Number of words/record (=identifier+rem.)
C-             LSUPP    [I]     Address of support bank if > 0.
C-             IZLINK   [I]     Link from which to hang bank.
C-
C-   Outputs : None
C-
C-                         Error codes. Use EZERR to check for code.
C-                         0 --- OK
C-                         1 --- Bank has been expanded at least once
C-                        -4 --- FATAL ERROR. IZLINK link already occupied
C-                        -5 --- Maximum bank size reached.
C-
C-      NOTE 1: The RCP bank is created stand-alone if LSUPP is
C-              zero; otherwise LSUPP will be taken as the address of
C-              the supporting bank from which the RCP bank is to be
C-              hung. IZLINK specifies the link.
C-
C-      NOTE 2: The RCP bank will be automatically expanded to accomodate
C-              new entries. Maximum number of records per bank is given by
C-              MAXIDS (2000). A record corresponds either to a parameter
C-              IDENTIFIER (perhaps with a COMMENT), a BLANK line, or to
C-              a COMMENT line. Arrays specified by an array block
C-
C-                      \ARRAY array-name [! comment]
C-                              :
C-                      \END
C-
C-              allow for the input of parameter identifiers associated
C-              with more than one value. The absolute total number of
C-              values which can be stored in an RCP bank is determined
C-              by MAXVAL (65535). Arrays can consist of a mixture of
C-              values of different type.
C-
C-      NOTE 3: If the command \SIZE is placed in the FIRST line of the file
C-              the number following it will be used to allocate the initial
C-              number of place holders for values. The second number, if
C-              present, will be used to allocate the initial number of
C-              place holders for records (identifiers + comment lines).
C-              If the \SIZE command is not used then the following defaults
C-              are assumed:
C-
C-                      DEFVAL = 4000   Number of values
C-                      DEFIDS =  200   Number of records
C-
C-   Created  26-NOV-1987   Rajendran Raja
C-   Updated  12-NOV-1988   Harrison B. Prosper
C-                          Major change: New RCP bank format used.
C-   Updated  15-DEC-1988   Harrison B. Prosper
C-                          Moved packing code into routine EZZIRC
C-   Updated  30-JUN-1989   Harrison B. Prosper
C-   Moved booking code into EZZBK
C-   Updated  22-MAR-1991   Harrison B. Prosper
C-      Note ISRCP and reset at end
C-   Updated  26-MAR-1991   Harrison B. Prosper
C-      Postpone enforcement of EZPICK rule until further notice
C-   Updated  11-JUL-1991   Harrison B. Prosper
C-      Propagate all error codes correctly.
C-   Updated   4-Sep-1993   Herbert Greenlee
C-      Added memory-to-memory option (EZ_GET_FIFO).
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER       LUN
      CHARACTER*(*) BKNAME
      INTEGER       WRDREC
      INTEGER       LSUPP
      INTEGER       IZLINK
C
      INTEGER LRCP,LPTI,LPTO,LPTV,LPTT
      INTEGER IER,II,JJ,I,J,K,L,OLD_ISRCP,OLD_IER,EOF
      LOGICAL EZERROR,ACTIVE
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:SRCP.DEF'
      INCLUDE 'D0$INC:LKSRCP.INC'
C----------------------------------------------------------------------
      CHARACTER*(CHRCRD) RECORD
C----------------------------------------------------------------------
C      OLD_ISRCP = ISRCP                 ! Note previous ISRCP
C
C ****  READ FIRST RECORD and determine initial bank size
C ****  and book RCP bank
C
      IF(LUN.GE.0)THEN
        READ(LUN,FMT='(A)') RECORD
      ELSE
        CALL EZ_GET_FIFO(RECORD, EOF)
      ENDIF
C
C ****  Skip \START if present
C
      CALL WORD (RECORD,I,J,K)
      IF ( RECORD(I:J) .EQ. '\START' ) THEN
        IF(LUN.GE.0)THEN
          READ(LUN,FMT='(A)') RECORD
        ELSE
          CALL EZ_GET_FIFO(RECORD, EOF)
        ENDIF
      ENDIF
C
      L    = LEN(BKNAME)
      CALL EZZBK (RECORD,BKNAME(1:L),WRDREC,LSUPP,IZLINK,
     &  LRCP,LPTI,LPTO,LPTV,LPTT)
      IF ( EZERROR(IER) ) GOTO 999
C
C ****  Loop over REMAINING records in file
C
      ACTIVE = .TRUE.
      DO WHILE ( ACTIVE )
        IF(LUN.GE.0)THEN
          READ(LUN,FMT='(A)',END=60) RECORD
        ELSE
          CALL EZ_GET_FIFO(RECORD, EOF)
          IF(EOF.NE.0)GO TO 60
        ENDIF
C
        CALL EZZIRC (RECORD,LRCP,LPTI,LPTO,LPTV,LPTT,IER)
C
        ACTIVE = (IER .EQ. EZS_SUCCESS) .OR.
     &           (IER .EQ. EZS_BANK_EXTENDED)
      ENDDO
C
C ****  Complete construction of RCP bank
C
   60 CONTINUE
      OLD_IER = IER                      ! Note code from last step
      CALL EZEND
      IF ( .NOT. EZERROR(IER) ) THEN
        IER = OLD_IER                    ! Return code from decoding stage
      ENDIF
C
C ****  Ignore End-Of-File and End-Of-Data
C
      IF ( (IER .EQ. EZS_ENDOF_FILE) .OR.
     &     (IER .EQ. EZS_ENDOF_DATA) ) THEN
        IER = EZS_SUCCESS
      ENDIF
C
C ****  Print out errors
C
      IF ( IER .NE. EZS_SUCCESS ) THEN
        CALL EZGET_ERROR_TEXT(IER,RECORD)
        CALL ERRMSG('EZ_ERROR','EZREAD',RECORD,'W')
      ENDIF
C
C      ISRCP = OLD_ISRCP                 ! Reset to previous value
  999 RETURN
      END
