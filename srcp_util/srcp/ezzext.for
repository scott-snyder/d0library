      SUBROUTINE EZZEXT (NEWIDS,NEWVAL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Extend pre-selected SRCP bank by NEWIDS
C-                         records for identifiers and by NEWVAL words
C-                         for values.
C-
C-   Inputs  : NEWIDS      Number of new identifiers records
C-             NEWVAL      Number of new value records
C-   Outputs : None
C-   Controls: None
C-
C-   Created   3-OCT-1988   Harrison B. Prosper
C-   Modified 16-NOV-1988   Harrison B. Prosper
C-                          MAJOR CHANGE: Uses NEW SRCP bank format
C-   Updated  15-MAY-1990   Harrison B. Prosper   
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER ID,I,J,L,N,NEWIDS,NEWVAL,NWORDS,WRDIDS,NUMIDS,NUMVAL
      INTEGER LIDS,LVAL,LPTV,LPTO,LPTT,LPTV1,LPTO1,LPTT1
C
      INCLUDE 'D0$LINKS:IZSRCP.LINK'
      INCLUDE 'D0$PARAMS:SRCP.DEF'
      INCLUDE 'D0$INC:LKSRCP.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C----------------------------------------------------------------------
C
C       ISRCP ---- Pointer to currently selected bank
C
C ****  Clear error flag
C
      ERRSRC = EZS_SUCCESS
C
      IF ( ISRCP .GT. 0 ) THEN
C
C ****  Get current bank size parameters from SRCP bank
C
        LSRCP  = KSRCP(ISRCP)
        LIDS   = IC(LSRCP+JJIDS)   ! Number of identifiers in SRCP bank
        LVAL   = IC(LSRCP+JJVAL)   ! Number of values in SRCP bank
        WRDIDS = IC(LSRCP+JJNWRD)  ! Number of fullwords/identifier
        NUMIDS = IC(LSRCP+JJNIDS)  ! Current Maximum number of ids/bank
        NUMVAL = IC(LSRCP+JJNVAL)  ! Current Maximum number of val/bank
C
C ****  Update record count in SRCP header bank
C
        NUMIDS = NUMIDS + NEWIDS
        IC(LSRCP+JJNIDS) = NUMIDS
C
        NUMVAL = NUMVAL + NEWVAL
        IC(LSRCP+JJNVAL) = NUMVAL
C
C ****  Add records to bank
C
        NWORDS = WRDIDS*NEWIDS + NEWIDS + NEWVAL + NEWVAL
        CALL MZPUSH (IXSTP,LSRCP,0,NWORDS,'I')
        KSRCP(ISRCP) = LSRCP
C
C ****  Get CURRENT absolute pointers to END (+1) of Values and Order
C       list areas
C
        LPTO = LSRCP + IC(LSRCP+JJPORD) + LIDS
        LPTV = LSRCP + IC(LSRCP+JJPVAL) + LVAL
        LPTT = LSRCP + IC(LSRCP+JJPTYP) + LVAL
C
C ****  Update pointers in SRCP bank
C
        IC(LSRCP+JJPORD) = IC(LSRCP+JJPIDS)+WRDIDS*NUMIDS ! Pointer to ord. list
        IC(LSRCP+JJPVAL) = IC(LSRCP+JJPORD)+NUMIDS    ! Pointer to values
        IC(LSRCP+JJPTYP) = IC(LSRCP+JJPVAL)+NUMVAL    ! Pointer to value type
C
C ****  Get NEW absolute pointers to END (+1) of Values and Order
C       list areas
C
        LPTO1 = LSRCP + IC(LSRCP+JJPORD) + LIDS
        LPTV1 = LSRCP + IC(LSRCP+JJPVAL) + LVAL
        LPTT1 = LSRCP + IC(LSRCP+JJPTYP) + LVAL
C
C ****  Move type list THEN values THEN order list to new area boundaries
C
        IF ( LPTT1 .NE. LPTT ) THEN
          DO 10 I = 1,LVAL
            IC(LPTT1-I) = IC(LPTT-I)
   10     CONTINUE
          DO 20 I = 1,LVAL
            IC(LPTV1-I) = IC(LPTV-I)
   20     CONTINUE
        ENDIF
C
        IF ( LPTO1 .NE. LPTO ) THEN
          DO 30 I = 1,LIDS
            IC(LPTO1-I) = IC(LPTO-I)
   30     CONTINUE
        ENDIF
C
      ELSE
        ERRSRC = EZS_BANK_NOTSELECTED
      ENDIF
C
  999 RETURN
      END
