      SUBROUTINE EZDECODE (LUN,NAME,VALS,TYPES,TOTAL,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-      Read and decode next parameter in an RCP file and return
C-      the value(s) and value type(s). The RCP file must be opened
C-      externally. This routine may be called in a loop to return
C-      all the parameters within an RCP file without creating an
C-      RCP bank.
C-
C-   Inputs  : LUN      [I]     Input unit number (opened externally)
C-
C-   Outputs : NAME     [C*]    Name of parameter
C-             VALS(*)  [R*]    Parameter values. Use equivalences
C-                              to convert between REAL, INTEGER and
C-                              LOGICAL. Use routine DHTOC convert
C-                              from REAL to CHARACTER.
C-             TYPES(*) [I*]    Value types (see RCP_MANUAL).
C-             TOTAL    [I]     Total number of values/parameter name
C-             IER      [I]     non-zero for End-Of-Data.
C-
C-   Controls: None
C-
C-   Created  13-SEP-1989   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER       LUN
      CHARACTER*(*) NAME
      REAL          VALS(*)
      INTEGER       TYPES(*)
      INTEGER       TOTAL
      INTEGER       IER
C
      INTEGER LENCHR,IINAME,KKNAME,IIREM,JJREM,I,J,K
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:SRCP.DEF'
C----------------------------------------------------------------------
      CHARACTER*(CHRCRD)  RECORD
C----------------------------------------------------------------------
C
      TOTAL = 0
      IER = 0
C
  100 CONTINUE

      READ (LUN,FMT='(A)',END=900) RECORD
C
C ****  Do preliminary decoding of record
C
      CALL EZZDRC (RECORD,NAME,IINAME,KKNAME,LENCHR,IIREM)
C
      IF ( NAME(1:LENCHR) .EQ. '\START' .OR.
     &     NAME(1:1)      .EQ. '!'      .OR.
     &     NAME(1:1)      .EQ. ' '      .OR.
     &     NAME(1:LENCHR) .EQ. '\SIZE' ) GOTO 100
C
C ****  Decode RECORD
C
      CALL EZZDEC (RECORD,VALS,TYPES,TOTAL,IER)
      IF ( IER   .NE. EZS_SUCCESS ) GOTO 999      ! Check for End-of-Data
      IF ( TOTAL .GT. 0 ) GOTO 950
      GOTO 100                          ! Get next record
C
  900 CONTINUE                          ! End-of-File
      IER = EZS_ENDOF_FILE
      NAME = ' '
      RETURN
C
  950 CONTINUE                          ! Here if TOTAL > 0
      CALL WORD (RECORD,I,J,K)
      NAME = RECORD(I:J)
  999 RETURN
      END
