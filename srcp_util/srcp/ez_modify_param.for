      SUBROUTINE EZ_MODIFY_PARAM(ARRAY_NAME,ARRAY_ELEMENT,IVAL,CVAL,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Modifies the Nth parameter in a give array.
C-   It is assume that value that will replace the requested parameter
C-   will be of the same type of the previous one.
C-
C-   Inputs  : ARRAY_NAME    [C*]: Nane of the array
C-             ARRAY_ELEMENT [C*]: Number of the element to be modify
C-             IVAL          [I ]: New value of the element
C-             CVAL          [C*]: New value of the element if character
C-
C-   Outputs : IER           [I ]: 0 If successfull
C-
C-   Created 11-JUN-1991   Lupe Howell
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) ARRAY_NAME
      CHARACTER*(*) ARRAY_ELEMENT
      CHARACTER*(*) CVAL
      INTEGER IVAL,IER
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:SRCP.DEF'
      INCLUDE 'D0$INC:LKSRCP.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$OFFLINE_UTIL$GENERAL:VALUE.DEF'
C----------------------------------------------------------------------
      INTEGER MAXREC
      PARAMETER( MAXREC = 500 )
C
      INTEGER IIVAL
      REAL    RRVAL
      LOGICAL LLVAL
      EQUIVALENCE(IIVAL,RRVAL,LLVAL)
C
      INTEGER NEW_IVAL
      REAL    NEW_RVAL
      LOGICAL NEW_LVAL
      EQUIVALENCE(NEW_IVAL,NEW_RVAL,NEW_LVAL)
C
      INTEGER ID,ELEN,START,IEND,CLEN
      INTEGER TOTAL,IPOINT,VALPOINT,RECORD_COUNT
      INTEGER EZZSHFT,EZZAND,IPVAL,IPTV,IPTT,IPTO,IPTI
      INTEGER LARR,LVAL,ITYP,I,J,ELEMENT_REQUESTED
C
      CHARACTER*1 TICK
      CHARACTER*132 AVAL,BUFFER(MAXREC),CNUMBER,NEW_NUMBER
C
      LOGICAL ACTIVE
C
C----------------------------------------------------------------------
      LARR = LEN(ARRAY_NAME)
      CALL SWORDS(ARRAY_ELEMENT,START,IEND,ELEN)
      READ( ARRAY_ELEMENT(START:IEND), * ) ELEMENT_REQUESTED
C
C ****  Check if an SRCP bank has been selected
C
      IF ( ISRCP .LE. 0 ) THEN
        IER = EZS_BANK_NOTSELECTED
        ERRSRC = IER
        GOTO 999
      ENDIF
C
C ****  Get the bank address
C
      LSRCP = KSRCP(ISRCP)
      IF ( LSRCP .LE. 0 ) THEN
        IER = EZS_BANK_NOTFOUND
        ERRSRC = IER
        GOTO 999
      ENDIF
C
C ****  Get pointers to data within SRCP bank
C
      CALL EZZGPT (LSRCP,IPTI,IPTO,IPTV,IPTT)
C
C ****  Get index into RCP bank for the given array-name
C
      CALL EZGETI (ARRAY_NAME(1:LARR),ID,IER)
C
C ****  Get number of values/identifier from type list
C
      IPVAL = EZZSHFT(IC(IPTO+ID),-NBITS)       ! Pointer to values-list
      TOTAL = EZZAND(IC(IPTT+IPVAL),MASK)     ! Zero upper word
C
C ****  Make pointers point to absolute
C
      IPTV = IPTV + IPVAL
      IPTT = IPTT + IPVAL

   30 CONTINUE
      IPOINT = 1
      IER    = 0
      RECORD_COUNT = 0
      NEW_IVAL    = IVAL
C
C ****  Setting up the buffer with '\ARRAY' keyword
C
      RECORD_COUNT = RECORD_COUNT + 1
      BUFFER(RECORD_COUNT) = '\ARRAY '//ARRAY_NAME(1:LARR)

C
C ****  Searching for the Nth element requested
C
      IER = -1                          ! Assume it does not finds it
      ACTIVE = .TRUE.
      TICK = CHAR(39)
      DO WHILE ( ACTIVE )
C
C ****  Exit if we've gone beyond the given array
C
        IF ( IPOINT .GT. TOTAL ) GOTO 800
C
C ****  Get element of array and update pointer
C ****  and increment the counter
C
        CALL EZGET_NEXT_VALUE_TYPE(ARRAY_NAME,
     &          IIVAL,AVAL,ITYP,LVAL,IER,IPOINT)
        RECORD_COUNT = RECORD_COUNT + 1
C
C ****  Convert the elements of the array into
C ****  a string
C
        IF     ( ITYP .EQ. VTINT ) THEN    ! Integer
          WRITE(UNIT=CNUMBER,FMT='(I10,14X)')  IIVAL
        ELSEIF ( ITYP .EQ. VTLOG ) THEN        ! Logical
          IF ( LLVAL ) THEN
            WRITE(UNIT=CNUMBER,FMT='(5X,A5,14X)') ' TRUE'
          ELSE
            WRITE(UNIT=CNUMBER,FMT='(5X,A5,14X)') 'FALSE'
          ENDIF
        ELSEIF ( ITYP .EQ. VTREAL ) THEN       ! Real
          WRITE(UNIT=CNUMBER,FMT='(F10.4,14X)') RRVAL
        ELSEIF ( ITYP .GT. VTCHR ) THEN        ! Character
          CNUMBER = TICK//AVAL(1:LVAL)//TICK
        ELSE
          CNUMBER = ' '
        ENDIF
C
C ****  Comparing current parameter count with the number requested
C ****  If the counter equals to the requested number replace the
C ****  current record with the new record
C
        IF ( ELEMENT_REQUESTED .EQ. ( RECORD_COUNT-1) ) THEN
C
C ****  Convert the new value to a string
C
          IF     ( ITYP .EQ. VTINT ) THEN    ! Integer
            WRITE(UNIT=NEW_NUMBER,FMT='(I10,14X)')  NEW_IVAL
          ELSEIF ( ITYP .EQ. VTLOG ) THEN        ! Logical
            IF ( NEW_LVAL ) THEN
              WRITE(UNIT=NEW_NUMBER,FMT='(5X,A5,14X)') ' TRUE'
            ELSE
              WRITE(UNIT=NEW_NUMBER,FMT='(5X,A5,14X)') 'FALSE'
            ENDIF
          ELSEIF ( ITYP .EQ. VTREAL ) THEN       ! Real
            WRITE(UNIT=NEW_NUMBER,FMT='(F10.4,14X)') NEW_RVAL
          ELSEIF ( ITYP .GT. VTCHR ) THEN        ! Character
            CALL SWORDS(CVAL,START,IEND,I)
            NEW_NUMBER = TICK//CVAL(START:IEND)//TICK
          ELSE
            NEW_NUMBER = ' '
          ENDIF
          BUFFER(RECORD_COUNT) = NEW_NUMBER
          IER = 0
        ELSE
          BUFFER(RECORD_COUNT) = CNUMBER
        ENDIF
      ENDDO
C
C ****  Setting up the buffer with '\END' keyword
C
  800 RECORD_COUNT = RECORD_COUNT + 1
      BUFFER(RECORD_COUNT) = '\END'
C
C ****  Recreate the array
C
      CALL EZ_MODIFY_ARRAY(ARRAY_NAME,BUFFER,RECORD_COUNT,IER)

  999 RETURN
      END
