      SUBROUTINE EZZCHK (NVAL,LSRCP,LPTI,LPTO,LPTV,LPTT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Check if there is enough space in SRCP bank
C-                         for 1 more record with NVAL values and if
C-                         not expand bank accordingly.
C-
C-   Inputs  : NVAL        Number of values
C-
C-   Outputs : LSRCP       Address of SRCP bank
C-             LPTI        Pointer to identifiers
C-             LPTO        Pointer to order list
C-             LPTV        Pointer to value list
C-             LPTT        Pointer to type list
C-
C-                         Use EZERR  to check for errors and to
C-                         return error code
C-                         0 --- OK
C-                         See also EZGET_ERROR_TEXT
C-   Controls: None
C-
C-   Created  10-NOV-1988   Harrison B. Prosper
C-   Updated  15-MAY-1990   Harrison B. Prosper   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL EXPAND
      INTEGER      NVAL,II,JJ,NN,NUMVAL,NUMIDS
      INTEGER      LIDS,LVAL,LPTI,LPTO,LPTV,LPTT,IPTO,IPTR
      INTEGER      I,J,K,L,M,N,NEWIDS,NEWVAL
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
C ****  Check if an SRCP bank has been selected.
C
      IF ( ISRCP .LE. 0 ) THEN
        ERRSRC = EZS_BANK_NOTSELECTED
        GOTO 999
      ENDIF
C
C ****  Check NVAL
C
      IF ( NVAL .LE. 0 ) THEN
        ERRSRC = EZS_BAD_ARGUMENT
        GOTO 999
      ENDIF
C
C ****  Get Bank size parameters
C
      LSRCP = KSRCP(ISRCP)
      LIDS   = IC(LSRCP+JJIDS)   ! Current number of records
      LVAL   = IC(LSRCP+JJVAL)   ! Current number of values
      NUMIDS = IC(LSRCP+JJNIDS)  ! Current maximum number of ids
      NUMVAL = IC(LSRCP+JJNVAL)  ! Current maximum number of values
      NEWIDS = 0
      NEWVAL = 0
      EXPAND = .FALSE.
C
C ****  Check if we have enough room to add new record
C       and determine by how much to expand bank
C
      IF ( (LIDS+1) .GT. NUMIDS ) THEN
        IF ( (LIDS+1) .LE. MAXIDS ) THEN
          EXPAND = .TRUE.
          NEWIDS = 1 + NUMIDS/2
          IF ( (LIDS+NEWIDS) .GT. MAXIDS ) NEWIDS = 1
        ELSE
          ERRSRC = EZS_MAX_PARAMS ! To signal maximum limit reached
          GOTO 999
        ENDIF
      ENDIF
C
C ****  Check if we have enough room to add new values
C
      IF ( (LVAL+NVAL) .GT. NUMVAL ) THEN
        IF ( (LVAL+NVAL) .LE. MAXVAL ) THEN
          EXPAND = .TRUE.
          NEWVAL = NVAL + NUMVAL/2
          IF ( (LVAL+NEWVAL) .GT. MAXVAL ) NEWVAL = NVAL
        ELSE
          ERRSRC = EZS_MAX_VALUES ! To signal maximum limit reached
          GOTO 999
        ENDIF
      ENDIF
C
      IF ( EXPAND ) THEN
        CALL EZZEXT (NEWIDS,NEWVAL)
        ERRSRC = EZS_BANK_EXTENDED ! To signal a bank extension
C
C ****  Define NEW base pointers into SRCP bank
C
        LSRCP = KSRCP(ISRCP)
        LPTI = LSRCP+IC(LSRCP+JJPIDS)-1 ! Base pointer to records
        LPTO = LSRCP+IC(LSRCP+JJPORD)-1 ! Base pointer to order list
        LPTV = LSRCP+IC(LSRCP+JJPVAL)-1 ! Base pointer to values
        LPTT = LSRCP+IC(LSRCP+JJPTYP)-1 ! Base pointer to type list
      ENDIF
C
  999 RETURN
      END
