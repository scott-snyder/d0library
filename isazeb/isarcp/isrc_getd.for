      SUBROUTINE ISRC_GETD (LISRC,ID,NUMBER,TYPE,TOTAL,RECORD,LENREC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return all the data associated with the
C-                         given parameter ID from the ISRC bank.
C-                         IMPORTANT: both reals and
C-                         integers are returned as floating point
C-                         numbers; no equivalencing is required.
C-                         However, you must DHTOC (UHTOC) to convert
C-                         to characters.
C-
C-   Inputs  : LISRC    [I]     ISRC address
C-             ID       [I]     Identifier index
C-
C-   Outputs : NUMBER(*)[R*]    Value(s) (Returned as REAL)
C-             TYPE(*)  [I*]    Value type
C-             TOTAL    [I]     Number of values (INTEGER)
C-             RECORD   [C*]    Stored record (CHARACTER)
C-             LENREC   [I]     Length of stored record
C-
C-   Controls: None
C-
C-   Created  12-JAN-1990   Chip Stewart,  Harrison B. prosper
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) RECORD
      REAL          NUMBER(*)
      INTEGER       LENREC,TOTAL,ID,TYPE(*),WRDIDS,IPVAL,LISRC
      INTEGER       LIDS,IPTI,IPTO,IPTV,IPTT,IPNT,II,I,J,LSRCP
C
      INCLUDE 'D0$PARAMS:SRCP.DEF'
      INCLUDE 'D0$INC:ZEBCOM.INC'
C----------------------------------------------------------------------
C
      TOTAL = 0
C
C ****  Get the bank address
C
      LSRCP = LISRC
      IF ( LSRCP .LE. 0 ) THEN
        GOTO 999
      ENDIF
C
C ****  Check value of identifier index
C
      LIDS = IQ(LSRCP+JJIDS)  ! Number of identifiers
      IF ( (ID .LT. 1 ) .OR. (ID .GT. LIDS) ) THEN
        GOTO 999
      ENDIF
C
C ****  Get pointers to data within SRCP bank
C
      IPTI = LSRCP+IQ(LSRCP+JJPIDS)-1 ! Identifiers
      IPTO = LSRCP+IQ(LSRCP+JJPORD)-1 ! Order list
      IPTV = LSRCP+IQ(LSRCP+JJPVAL)-1 ! Values
      IPTT = LSRCP+IQ(LSRCP+JJPTYP)-1 ! Type
C
C ****  Extract record from bank
C
      WRDIDS = IQ(LSRCP+JJNWRD)
      LENREC = WRDIDS*4
      IPNT = IPTI + WRDIDS*(ID-1)
      CALL UHTOC (IQ(IPNT+1),4,RECORD,LENREC)
C
C ****  Get number of values/identifier from type list
C
      IPVAL = ISHFT(IQ(IPTO+ID),-NBITS)
      TOTAL = IQ(IPTT+IPVAL) .AND. MASK
C
C ****  Get values and value types
C
      DO 410 II =  1,TOTAL
        J = IPVAL+II-1
        TYPE(II) = ISHFT(IQ(IPTT+J),-NBITS)
        IF ( TYPE(II) .EQ. 1 ) THEN     ! Integer type
          NUMBER(II) = IQ(IPTV+J)
        ELSE
          NUMBER(II) = Q(IPTV+J)
        ENDIF
  410 CONTINUE
C
  999 RETURN
      END
