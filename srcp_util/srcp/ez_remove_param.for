      SUBROUTINE EZ_REMOVE_PARAM(ARRAY_NAME,ARRAY_ELEMENT,OCURRENCE,
     &   NREC,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Remove NREC records in an RCP array.
C-   It will search in the array for the requested ARRAY_ELEMENT up to the
C-   OCCURRENCE requested and then it will the delete the number of records
C-   NREC requested including the ARRAY_ELEMENT. If the ARRAY_ELEMENT is equal
C-   to the control word 'INDEX' the routine will interpret OCURRENCE  as
C-   the index in the array where to start removing parameters.  It will
C-   remove NREC number of paramters after the given index.
C-
C-   Inputs  : ARRAY_NAME    [C*] - Name of the array
C-             ARRAY_ELEMENT [C*] - Name of the element to remove
C-                                  or the Key Word 'INDEX'
C-             OCURRENCE     [I ] - The nth occurrence of the element
C-                                  specified in ARRAY_ELEMENT or
C-                                  the index where to start to remove
C-             NREC          [I ] - Number of records to be remove
C-
C-   Outputs : IER           [I ] - 0 - OK
C-
C-   Created  14-AUG-1991   Lupe Howell
C-   Updated  12-DEC-1991   Lupe Howell   The use of index to remove parameters
C-                          allowed.
C-   Updated  16-DEC-1991   Harrison B. Prosper  
C-      Use symbolic constants. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) ARRAY_NAME
      CHARACTER*(*) ARRAY_ELEMENT
      INTEGER NREC, OCURRENCE, IER
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
c
      INTEGER COUNT,ID,II,JJ,LPAR,LREM
      INTEGER TOTAL,IPOINT,VALPOINT,RECORD_COUNT
      INTEGER LEN,EZZSHFT,EZZAND,IPVAL,IPTV,IPTT,IPTO,IPTI
      INTEGER LRCP,LARR,LNAM,JVAL,LVAL,ITYP,I,J,K,N,L
      INTEGER KK,LL,JJJ,KKK,LLL,PARAM_INDEX
C
      CHARACTER*(NUMCHR) NAME
      CHARACTER*(NUMCHR) PARNAME
      CHARACTER*(CHRCRD) AVAL,REM,BUFFER(MAXREC),STRING,CNUMBER,
     &  CURRENT_VAL
      CHARACTER*(CHRCRD) TEMP
      CHARACTER*1 TICK
C
      LOGICAL ACTIVE,GET,CHARS,INDEX,REMOVE
C----------------------------------------------------------------------
      LARR = LEN(ARRAY_NAME)
      CALL SWORDS(ARRAY_ELEMENT,I,J,LNAM)
      TICK = CHAR(39)
C
C ****  Check if the remove has to be done by INDEX
C
      IF ( ARRAY_ELEMENT(1:LNAM) .EQ. 'INDEX' ) THEN
        INDEX = .TRUE.
      ELSE
        INDEX = .FALSE.
      ENDIF
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
      COUNT  = 0
      IER    = 0
      RECORD_COUNT = 0
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
      REMOVE = .FALSE.
      DO WHILE ( ACTIVE )
C
C ****  Exit if we've gone beyond the given array
C
        IF ( IPOINT .GT. TOTAL ) GOTO 888

C
C ****  Get element of array and update pointer
C ****  and increment the counter
C
        PARAM_INDEX = IPOINT
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
          CURRENT_VAL = AVAL(1:LVAL)
          I = 1
          J = LVAL + 2
        ELSE
          CNUMBER = ' '
        ENDIF
        IF ( ITYP .LT. VTCHR ) THEN
          CURRENT_VAL = CNUMBER
          CALL SWORDS(CURRENT_VAL,I,J,K)
        ENDIF
        BUFFER(RECORD_COUNT) = CNUMBER(I:J)
C
C ****  If index search was requested check if the current parameter 
C ****  matches the requested index to start remove
C
        IF ( INDEX ) THEN
          IF ( PARAM_INDEX .EQ. OCURRENCE ) THEN
            REMOVE = .TRUE.
          ENDIF
C
C ****  If no index search was requested compare current parameter name 
C ****  with parameter name requested.   If the counter equals to the 
C ****  number of ocurrence requested start remove 
C
        ELSEIF ( ARRAY_ELEMENT(1:LNAM) .EQ. CURRENT_VAL(I:J) ) THEN
          COUNT = COUNT + 1
          IF( COUNT .EQ. OCURRENCE ) THEN
            REMOVE = .TRUE.
          ENDIF
        ENDIF
C
C ****  If a REMOVE was set remove the paramter read and the number of 
C ****  records requested.
C
        IF ( REMOVE ) THEN
          STRING = BUFFER(RECORD_COUNT)
          RECORD_COUNT = RECORD_COUNT - 1
C
C ****  Skip NREC records to remove them from the buffer
C
          I = 0
          DO WHILE( ( I .LT. (NREC-1) ) .AND. (IPOINT .LT. TOTAL ) )
            I = I + 1
            CALL EZGET_NEXT_VALUE_TYPE(ARRAY_NAME,
     &          IIVAL,AVAL,ITYP,LVAL,IER,IPOINT)
          ENDDO
          REMOVE = .FALSE.
        ENDIF
      ENDDO
  888 CONTINUE
C
C ****  Setting up the buffer with '\END' keyword
C
      RECORD_COUNT = RECORD_COUNT + 1
      BUFFER(RECORD_COUNT) = '\END'
C
C ****  Recreate the array
C
      CALL EZ_MODIFY_ARRAY(ARRAY_NAME,BUFFER,RECORD_COUNT,IER)

  999 RETURN
      END
