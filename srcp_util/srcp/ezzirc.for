      SUBROUTINE EZZIRC (RECORD,LSRCP,LPTI,LPTO,LPTV,LPTT,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-      Insert a single line which may be part of a multi-line record
C-      into the SRCP bank at address LSRCP. Note, however, that while
C-      single line records are inserted immediately, multiple line records
C-      specified with the \ARRAY....\END construct are batched and are
C-      inserted into the bank only when the \END has been received.
C-
C-      NOTE:   Since the bank size can increase dynamically the various
C-              addresses can change, in which case EZZIRC will return
C-              the updated values.
C-
C-   Inputs  : RECORD           A single line (Character*(*))
C-             LSRCP            Address of SRCP bank
C-             LPTI             Base address of identifier list
C-             LPTO             Base address of order list
C-             LPTV             Base address of value list
C-             LPTT             Base address of value-type list
C-
C-   Outputs : LSRCP..LPTT      New values returned if bank is expanded
C-   Controls: None
C-
C-   Created  15-DEC-1988   Harrison B. Prosper
C-   Updated  10-NOV-1989   Harrison B. Prosper
C-   Now decodes long strings not enclosed in a \ARRAY...\END
C-   Updated  14-DEC-1991   Harrison B. Prosper  
C-      Allow zero length arrays 
C-   Updated  15-JUL-1992   Harrison B. Prosper  
C-      Handle integers exactly 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      CHARACTER*(*)  RECORD
      INTEGER       LPTI,LPTO,LPTV,LPTT,IER
C
      LOGICAL EZERROR
      INTEGER NNVAL
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:SRCP.DEF'
      INCLUDE 'D0$INC:LKSRCP.INC'
      INCLUDE 'D0$INC:BFSRCP.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C----------------------------------------------------------------------
C
C ****  Decode RECORD
C
      CALL EZZDEC (RECORD(1:LEN(RECORD)),RVALUE,ITYPE,NNVAL,IER)
      IF ( NNVAL .EQ. 0 ) GOTO 999
      IF ( IER   .NE. EZS_SUCCESS ) GOTO 999
C
C ****  Check bank size and expand if necessary
C
      CALL EZZCHK (IABS(NNVAL),LSRCP,LPTI,LPTO,LPTV,LPTT)
      IF ( EZERROR(IER) ) THEN
        IF ( IER .NE. EZS_BANK_EXTENDED ) GOTO 999
      ENDIF
C
C ****  Store data in bank
C
      CALL EZZSTO (RECORD,RVALUE,ITYPE,NNVAL,
     &  LSRCP,LPTI,LPTO,LPTV,LPTT,0)
C
  999 RETURN
      END
