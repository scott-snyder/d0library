      SUBROUTINE EZGETN (ID,NAME,N)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return Name of parameter with index ID.
C-
C-   Inputs  : ID          Identifier index
C-
C-   Outputs : NAME        Name of parameter (32-chars. max)
C-             N           Number of characters in name
C-
C-                           Error codes. Use EZERR to check for errors.
C-                           0 --> OK
C-                           See also EZGET_ERROR_TEXT.
C-
C-   Controls: None
C-
C-   Created  13-NOV-1988   Harrison B. prosper
C-   Updated  18-SEP-1989   Harrison B. Prosper   
C-   Removed call to WORD
C-   Updated  15-MAY-1990   Harrison B. Prosper   
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER       ID
      CHARACTER*(*) NAME
C
      CHARACTER*32  IDENTF
      INTEGER       LENREC,WRDIDS,IPVAL
      INTEGER       LIDS,IPTI,IPTO,IPTV,IPTT,IPNT,II,I,J,N
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
      WRDIDS = IC(LSRCP+JJNWRD)
      IPTI = LSRCP+IC(LSRCP+JJPIDS)-1 ! Base address of ID-list
C
C ****  Extract record from bank
C
      IPNT = IPTI + WRDIDS*(ID-1)
      CALL UHTOC (IC(IPNT+1),4,IDENTF,NUMCHR)
      CALL WORD (IDENTF,I,J,N)
      NAME = IDENTF(I:J)
C
  999 RETURN
      END
