      SUBROUTINE EZGETD (ID,NUMBER,TYPE,TOTAL,RECORD,LENREC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return all the data associated with the
C-                         given parameter ID from the currently
C-                         selected RCP bank. IMPORTANT: Use equivalences
C-                         to convert from real to integer. Use
C-                         DHTOC (UHTOC) to convert to characters.
C-
C-   Inputs  : ID       [I]     Identifier index
C-
C-   Outputs : NUMBER(*)[R*]    Value(s) (Returned as REAL)
C-             TYPE(*)  [I*]    Value type
C-             TOTAL    [I]     Number of values (INTEGER)
C-             RECORD   [C*]    Stored record (CHARACTER)
C-             LENREC   [I]     Length of stored record
C-
C-                           Error codes. Use EZERR to check for errors.
C-                           0 --> OK
C-                           See also EZGET_ERROR_TEXT.
C-
C-   Controls: None
C-
C-   Created  13-NOV-1988   Harrison B. prosper
C-   Updated  11-MAY-1990   Harrison B. Prosper   
C-   Updated  15-JUL-1992   Harrison B. Prosper   
C-      Return integers exactly
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) RECORD
      REAL          NUMBER(*)
      INTEGER       LENREC,TOTAL,ID,TYPE(*),WRDIDS,IPVAL
      INTEGER       LIDS,IPTI,IPTO,IPTV,IPTT,IPNT,II,I,J
      INTEGER EZZAND,EZZSHFT
C
      INCLUDE 'D0$PARAMS:SRCP.DEF'
      INCLUDE 'D0$INC:LKSRCP.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C----------------------------------------------------------------------
C
C ****  Clear error flag etc.
C
      ERRSRC = EZS_SUCCESS
C
      TOTAL = 0
C
C ****  Check if an SRCP bank has been selected
C
      IF ( ISRCP .LE. 0 ) THEN
        ERRSRC = EZS_BANK_NOTSELECTED
        GOTO 999
      ENDIF
C
C ****  Get the bank address
C
      LSRCP = KSRCP(ISRCP)
      IF ( LSRCP .LE. 0 ) THEN
        ERRSRC = EZS_BANK_NOTFOUND
        GOTO 999
      ENDIF
C
C ****  Check value of identifier index
C
      LIDS = IC(LSRCP+JJIDS)  ! Number of identifiers
      IF ( (ID .LT. 1 ) .OR. (ID .GT. LIDS) ) THEN
        ERRSRC = EZS_BAD_ARGUMENT
        GOTO 999
      ENDIF
C
C ****  Get pointers to data within SRCP bank
C
      CALL EZZGPT (LSRCP,IPTI,IPTO,IPTV,IPTT)
C
C ****  Extract record from bank
C
      WRDIDS = IC(LSRCP+JJNWRD)
      LENREC = WRDIDS*4
      IPNT = IPTI + WRDIDS*(ID-1)
      CALL UHTOC (IC(IPNT+1),4,RECORD,LENREC)
C
C ****  Get number of values/identifier from type list
C
      IPVAL = EZZSHFT(IC(IPTO+ID),-NBITS)
      TOTAL = EZZAND(IC(IPTT+IPVAL),MASK)
C
C ****  Get values and value types
C
      DO 410 II =  1,TOTAL
        J = IPVAL+II-1
        TYPE(II)  = EZZSHFT(IC(IPTT+J),-NBITS)
        NUMBER(II)= C(IPTV+J)
  410 CONTINUE
C
  999 RETURN
      END
