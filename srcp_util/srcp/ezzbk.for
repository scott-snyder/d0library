      SUBROUTINE EZZBK (RECORD,BKNAME,WRDIDS,LSUPP,IZLINK,
     &  LSRCP,LPTI,LPTO,LPTV,LPTT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Book an SRCP bank and move it into an existing
C-                         structure of banks if requested. Insert the
C-                         specified record. This record may contain the
C-                         \SIZE command.
C-
C-   Inputs  : RECORD   [C*]    Record which, optionally, contains the
C-                              \SIZE command.
C-             BKNAME   [C*]    Bank name (32-characters maximum)
C-             WRDIDS   [I]     Number of 32-bit words/record. A record
C-                              in this context is an indentifier plus
C-                              an optional comment.
C-             LSUPP    [I]     Address of support bank if > 0.
C-             IZLINK   [I]     Link from which to hang SRCP bank.
C-
C-   Outputs : LSRCP    [I]     Address of SRCP bank
C-             LPTI...LPTT      Pointers into SRCP bank.
C-
C-   Controls: None
C-
C-   Created  30-JUN-1989   Harrison B. Prosper
C-   Updated  15-MAY-1990   Harrison B. Prosper
C-   Updated  13-JUL-1992   Harrison B. Prosper  
C-      Use EZSHUNT 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      CHARACTER*(*) RECORD
      CHARACTER*(*) BKNAME
      INTEGER       WRDIDS
      INTEGER       LSUPP
      INTEGER       IZLINK
C
      INTEGER       LPTI
      INTEGER       LPTO
      INTEGER       LPTV
      INTEGER       LPTT
C
      LOGICAL EZERR
      INTEGER LIDS,LENCHR,IER,IIREM
      INTEGER II,JJ,NUMIDS,NUMVAL,I,J,K,L,LREC
C
      REAL    VALUEX
      CHARACTER*32  NAME
      CHARACTER*132  REMARK
C
      INCLUDE 'D0$PARAMS:SRCP.DEF'
      INCLUDE 'D0$INC:LKSRCP.INC'
      INCLUDE 'D0$INC:NMSRCP.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C----------------------------------------------------------------------
C
C ****  Clear error flag
C
      ERRSRC = EZS_SUCCESS
C
C **** Look at RECORD and determine initial bank size
C
      LREC = LEN(RECORD)
C
      CALL EZZDRC (RECORD(1:LREC),NAME,II,JJ,LENCHR,IIREM)
C
      IF ( NAME(1:LENCHR) .EQ. '\SIZE' ) THEN
        NUMVAL = VALUEX (RECORD(JJ+1:LREC),I,J,K)
        NUMIDS = VALUEX (RECORD(JJ+J+1:LREC),I,K,L)
      ELSE
        NUMVAL = DEFVAL
        NUMIDS = DEFIDS
      ENDIF
      IF ( NUMVAL .LE. 0 ) NUMVAL = DEFVAL
      IF ( NUMIDS .LE. 0 ) NUMIDS = DEFIDS
C
C ****  Fatal error if number of records requested exceeds resources
C       available.
C
      IF ( NUMIDS .GT. MAXIDS ) THEN
        WRITE(REMARK,'(''Number of identifiers exceeds'',I6,''!!'')')
     &    MAXIDS
        CALL ERRMSG ('SRCP','EZZBK',REMARK,'F')
      ENDIF
      IF ( NUMVAL .GT. MAXVAL ) THEN
        WRITE(REMARK,'(''Number of values exceeds'',I6,''!!'')')
     &    MAXVAL
        CALL ERRMSG ('SRCP','EZZBK',REMARK,'F')
      ENDIF
C
C ****  Create stand-alone SRCP bank
C
      L = LEN(BKNAME)
      CALL EZBOOK (BKNAME(1:L),WRDIDS,NUMIDS,NUMVAL)
      IF ( EZERR  (IER) ) GOTO 999
C
C ****  Insert it below bank with address LSUPP if LSUPP > 0.
C
      IF ( LSUPP .GT. 0 ) THEN
        CALL EZSHUNT (BKNAME(1:L),LSUPP,IZLINK)
      ENDIF
C
C ****  Check error code
C
      IF ( EZERR  (IER) ) THEN
        CALL EZGET_ERROR_TEXT(IER,REMARK)
        CALL ERRMSG ('SRCP','EZZBK',REMARK,'F')
      ENDIF
C
      ERRSRC = EZS_SUCCESS ! Clear error flag
C
C ****  Get base pointers into SRCP bank
C
      CALL EZZGPT (LSRCP,LPTI,LPTO,LPTV,LPTT)
C
C ****  Save record if it does not contain \SIZE
C
      IF ( NAME(1:LENCHR) .NE. '\SIZE' ) THEN
        CALL EZZIRC (RECORD(1:LREC),LSRCP,LPTI,LPTO,LPTV,LPTT,IER)
      ENDIF
C
  999 RETURN
      END
