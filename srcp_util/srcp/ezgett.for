      SUBROUTINE EZGETT (ID,NAME,N,TYPE,SIZE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return Name, type and size of parameter with
C-                         specified parameter ID.
C-
C-   Inputs  : ID   [I]    Identifier index
C-
C-   Outputs : NAME [C*]   Name of parameter (32-chars. max)
C-             N    [I]    Number of characters in name
C-
C-             TYPE [I]    Type of identifier. For an array type
C-                         it will be the type of the FIRST element
C-                         in the array. Use the value of SIZE to 
C-                         identify an array (SIZE > 1).
C-             SIZE [I]    Number of values/identifier
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
C-      Use symbolic constants
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER       ID
      CHARACTER*(*) NAME
      INTEGER       N
      INTEGER       TYPE
      INTEGER       SIZE
C
      CHARACTER*32  IDENTF
      INTEGER       WRDIDS,IPVAL
      INTEGER       LIDS,IPTI,IPTO,IPTV,IPTT,IPNT,II,I,J
      INTEGER EZZAND,EZZSHFT
C
      INCLUDE 'D0$PARAMS:SRCP.DEF'
      INCLUDE 'D0$INC:LKSRCP.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
C----------------------------------------------------------------------
C
C ****  Clear error flag
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
      CALL EZZGPT (LSRCP,IPTI,IPTO,IPTV,IPTT)
C
C ****  Extract record from bank
C
      IPNT = IPTI + WRDIDS*(ID-1)
      CALL UHTOC (IC(IPNT+1),4,IDENTF,NUMCHR)
      NAME = IDENTF
C
C ****  Get number of values/identifier and type from type list
C
      IPVAL = EZZSHFT(IC(IPTO+ID),-NBITS)
      SIZE  = EZZAND(IC(IPTT+IPVAL),MASK)
      TYPE  = EZZSHFT(IC(IPTT+IPVAL),-NBITS)
C
  999 RETURN
      END
