      SUBROUTINE EZFILL (IDENTF,REMARK,NUMBER,TYPE,TOTAL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Write a record to previously selected RCP
C-                         bank. Use EZPICK to select bank. Call EZEND
C-                         after one or more calls to EZFILL in order to
C-                         complete construction of the bank. The RCP
C-                         bank will be automatically extended if there is
C-                         not enough room for the new record(s). This is
C-                         signaled by setting ERRSRC = 1; use EZERR to
C-                         return Error code. IMPORTANT: Use equivalences
C-                         to mix reals, integers and logicals and
C-                         use DCTOH (UCTOH) to convert from characters
C-                         to reals or integers.
C-
C-   Inputs  : IDENTF           [C*]    Name of identifier
C-                                      Up to 32 characters
C-             REMARK           [C*]    Short comment
C-             NUMBER(*)                Value(s)
C-             TYPE(*)          [C*]    Type of parameter
C-                                      I       INTEGER,
C-                                      R       REAL,
C-                                      L       LOGICAL,
C-                                      C       CHARACTER
C-
C-                              NOTE:   If two strings are consecutive
C-                              the last TYPE of the first string
C-                              should be replaced with either an & or
C-                              $ symbol to delimit the string.
C-
C-                              IMPORTANT:  The string length is rounded
C-                              up to nearest multiple of 4.
C-
C-             TOTAL            [C*]    Number of values
C-
C-   Outputs   None        Use EZERR to check for errors and to
C-                         return error code. See also
C-                         EZGET_ERROR_TEXT.
C-                         0 --- OK
C-   Controls: None
C-
C-   Created  27-SEP-1988   Harrison B. Prosper
C-   Modified 10-NOV-1988   Harrison B. Prosper
C-                          Uses new pointer scheme in which number of
C-                          values can exceed number of parameters
C-   Updated  13-NOV-1989   Harrison B. Prosper
C-      Handles variable length strings
C-   Updated  15-MAY-1990   Harrison B. Prosper   
C-      Use symbolic error codes
C-   Updated   5-NOV-1991   Krzysztof L. Genser   
C-      to handle FATMEN long generic names
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER      NIDENT,TOTAL
      CHARACTER*(*) REMARK
      CHARACTER*(*) IDENTF,TYPE(*)
      INTEGER       NUMBER(*)
C
      REAL    RNUMB
      INTEGER INUMB
      EQUIVALENCE (RNUMB,INUMB)
C
      INTEGER      II,JJ,NN,IER
      INTEGER      I,J,K,L,M,N,LREMAR,LIDENT
      INTEGER      LPTI,LPTO,LPTV,LPTT,IPTO,IPTI
C
      REAL         RSMALL,RBIG
      PARAMETER( RSMALL = 0.1 )
      PARAMETER( RBIG   = 10000. )
C
      CHARACTER*1  KIND
      CHARACTER*255  RECORD
      LOGICAL      EZERR
C
      INCLUDE 'D0$PARAMS:SRCP.DEF'
      INCLUDE 'D0$INC:BFSRCP.INC'
      INCLUDE 'D0$INC:LKSRCP.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$OFFLINE_UTIL$GENERAL:VALUE.DEF'
C
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
C ****  Check TOTAL
C
      IF ( TOTAL .LE. 0 ) THEN
        ERRSRC = EZS_BAD_ARGUMENT
        GOTO 999
      ENDIF
C
C ****  Get bank address
C
      LSRCP = KSRCP(ISRCP)
C
C ****  Compute actual base addresses for data records etc.
C
      CALL EZZGPT (LSRCP,LPTI,LPTO,LPTV,LPTT)
C
C ****  Check if we have enough room to add new parameter
C       and return new pointers into SRCP bank
C
      CALL EZZCHK (TOTAL,LSRCP,LPTI,LPTO,LPTV,LPTT)
      IF ( EZERR  (IER) ) THEN
        IF ( IER .LT. EZS_SUCCESS ) THEN
          GOTO 999 ! Not enough room to expand bank
        ENDIF
      ENDIF
C
C ******************************
C ****  BUILD RCP RECORDS   ****
C ******************************
C
      LIDENT = LEN(IDENTF)
      LREMAR = LEN(REMARK)
C
C ****  Append remark to record
C
      CALL WORD (REMARK(1:LREMAR),I,J,N)
      RECORD = IDENTF(1:LIDENT)
      IF ( N .GT. 0 ) THEN
        IF ( REMARK(1:1) .EQ. '!' ) THEN
          RECORD = RECORD(1:NUMCHR)//REMARK(1:LREMAR)
        ELSE
          RECORD = RECORD(1:NUMCHR)//'! '//REMARK(1:LREMAR)
        ENDIF
      ENDIF
C
      I = 0
  100 CONTINUE
      I = I + 1
      IF ( I .GT. TOTAL ) GOTO 200
C
      KIND = TYPE(I)(1:1)
      CALL UPCASE (KIND,KIND)
      INUMB = NUMBER(I)
C
      IF ( KIND .EQ. 'I' ) THEN
        ITYPE(I) = 1
      ELSEIF ( KIND .EQ. 'R' ) THEN
        IF ( (RNUMB .GE. RSMALL) .AND.
     &         (RNUMB .LE. RBIG) ) THEN
          ITYPE(I) = 2
        ELSE
          ITYPE(I) = 3
        ENDIF
      ELSEIF ( ( KIND .EQ. 'L' )   .OR.
     &           ( KIND .EQ. 'B' ) ) THEN
        ITYPE(I) = 4
      ELSEIF ( ( KIND .EQ. 'H' )   .OR.
     &           ( KIND .EQ. 'C' ) ) THEN
C
C ****  Get string length L
C
        L = 0
  150   CONTINUE
        L = L + 1
        KIND = TYPE(I+L)(1:1)           ! Check next type for a character
        CALL UPCASE (KIND,KIND)
        IF ( ( KIND .EQ. 'H' )   .OR.
     &           ( KIND .EQ. 'C' ) ) GOTO 150

        IF ( ( KIND .EQ. '$' )   .OR.   ! Check if this is end-of-string
     &           ( KIND .EQ. '&' ) ) L = L + 1
C
        I = I - 1                       ! Now pack ITYPE
        NN = 4*L                        ! Length of string
        DO 160 J =  1,L
          I = I + 1
          ITYPE(I) = VTCHR + NN
  160   CONTINUE
C
      ELSEIF ( ( KIND .EQ. '$' )   .OR.
     &           ( KIND .EQ. '&' ) )   THEN
        ITYPE(I) = VTCHR + 4
      ELSE
        ITYPE(I) = 0
      ENDIF
C
      GOTO 100
C
 200  CONTINUE
C
C ****  Store data in bank
C
      CALL EZZSTO
     &  (RECORD,NUMBER,ITYPE,TOTAL,LSRCP,LPTI,LPTO,LPTV,LPTT,0)
C
  999 RETURN
      END
