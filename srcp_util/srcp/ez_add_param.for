      SUBROUTINE EZ_ADD_PARAM(ARRAY_NAME,ARRAY_ELEMENT,OCURRENCE,SWITCH,
     &  RECORD,NREC,IER) 
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Adds a parameter to an array.
C-
C-   Inputs  : ARRAY_NAME    [C*]: Nane of the array
C-             ARRAY_ELEMENT [C*]: Name of the element in the array
C-                                 before or after which the new parameter
C-                                 is to be inserted.  If equal ' '
C-                                 the element is to be inserted at the end of
C-                                 the array.
C-             OCURRENCE     [I ]: The nth ocurrence of the element specified
C-                                 in ARRAY_ELEMENT
C-             SWITCH        [I ]: Control switch to indicate the direction
C-                                 of the insert. 0 before, 1 after, -1 bottom,
C-                                 999 at the top
C-             RECORD     [C*(*)]: Value(s) to be inserted
C-             NREC          [I ]: Number of records to be added
C-
C-   Outputs : IER           [I ]: 0 OK
C-
C-   Created  19-JUN-1991   Lupe Howell
C-   Updated   3-DEC-1991   Lupe Howell  The size of holding char variables
C-   increased 
C-   Updated  16-DEC-1991   Harrison B. Prosper  
C-      Use symbolic character lengths (NUMCHR and CHRCRD) 
C-   Updated  12-Feb-1992   Herbert Greenlee
C-      Fixed for UNIX.  (Eliminate concatenation argument)
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) ARRAY_NAME
      CHARACTER*(*) ARRAY_ELEMENT
      CHARACTER*(*) RECORD(*)
      INTEGER SWITCH, NREC, OCURRENCE, IER
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
      INTEGER COUNT,ID,LPAR,LREM,RLEN
      INTEGER TOTAL,IPOINT,VALPOINT,RECORD_COUNT
      INTEGER LEN,EZZSHFT,EZZAND,IPVAL,IPTV,IPTT,IPTO,IPTI
      INTEGER LRCP,LARR,LNAM,JVAL,LVAL,ITYP,I,J,N,II,JJ
      INTEGER TRULEN
C
      CHARACTER*(NUMCHR) NAME,RCPFILE
      CHARACTER*(CHRCRD) PARNAME
      CHARACTER*(CHRCRD) AVAL,REM,BUFFER(MAXREC),STRING
      CHARACTER*1 TICK
      CHARACTER*(CHRCRD) ERROR_MESSAGE
C
      LOGICAL ACTIVE,GET,CHARS,BEFORE,BOTTOM,FOUND,TOP
C
C----------------------------------------------------------------------
      LARR = LEN(ARRAY_NAME)
      CALL SWORDS(ARRAY_ELEMENT,I,J,LNAM)
      CALL SWORDS(RECORD(1),I,J,RLEN)
      TICK = CHAR(39)
C
C ****  Determine Control SWITCH
C
      BEFORE = .FALSE.
      BOTTOM = .FALSE.
      TOP = .FALSE.
      IF ( SWITCH .EQ. 0 ) THEN
        BEFORE = .TRUE.
      ELSEIF ( SWITCH .EQ. 1 ) THEN
        BEFORE = .FALSE.
      ELSEIF ( SWITCH .EQ. -1 ) THEN
        BOTTOM = .TRUE.
      ELSE
        TOP = .TRUE.
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
      FOUND = .FALSE.
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
      ACTIVE = .TRUE.
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
C
C ****  Convert the elements of the array into
C ****  a string
C
        IF     ( ITYP .EQ. VTINT ) THEN    ! Integer
          WRITE(UNIT=PARNAME,FMT='(I10,14X)')  IIVAL
        ELSEIF ( ITYP .EQ. VTLOG ) THEN        ! Logical
          IF ( LLVAL ) THEN
            WRITE(UNIT=PARNAME,FMT='(5X,A5,14X)') ' TRUE'
          ELSE
            WRITE(UNIT=PARNAME,FMT='(5X,A5,14X)') 'FALSE'
          ENDIF
        ELSEIF ( ITYP .EQ. VTREAL ) THEN       ! Real
          WRITE(UNIT=PARNAME,FMT='(F10.4,14X)') RRVAL
        ELSEIF ( ITYP .GT. VTCHR ) THEN        ! Character
          PARNAME = TICK//AVAL(1:LVAL)//TICK
        ELSE
          PARNAME = ' '
        ENDIF
C
C ****  If TOP check counter and insert in the top
C
        IF ( TOP ) THEN
          IF ( RECORD_COUNT .EQ. 1 ) THEN
            DO I = 1, NREC
              RECORD_COUNT = RECORD_COUNT + 1
              BUFFER(RECORD_COUNT) = RECORD(I)
              FOUND = .TRUE.
            ENDDO
          ENDIF
          RECORD_COUNT = RECORD_COUNT + 1
          BUFFER(RECORD_COUNT) = PARNAME
C
C ****  If the new parameter is to be placed at the bottom 
C ****  build the bufferd
C
        ELSEIF ( BOTTOM ) THEN
          RECORD_COUNT = RECORD_COUNT + 1
          BUFFER(RECORD_COUNT) = PARNAME
C
C ****  Comparing current parameter with parameter name requested
C ****  If the counter equals to the number of ocurrence requested
C ****  add the record(s) requested
C
        ELSE
          RECORD_COUNT = RECORD_COUNT + 1
          BUFFER(RECORD_COUNT) = PARNAME
          IF ( ARRAY_ELEMENT(1:LNAM) .EQ. PARNAME(1:LNAM) ) THEN
            COUNT = COUNT + 1
            IF( COUNT .EQ. OCURRENCE ) THEN
              FOUND = .TRUE.
              IF ( BEFORE ) THEN
                STRING = BUFFER(RECORD_COUNT)
                RECORD_COUNT = RECORD_COUNT - 1
                DO I =  1, NREC
                  RECORD_COUNT = RECORD_COUNT + 1
                  BUFFER(RECORD_COUNT) = RECORD(I)
                ENDDO
                RECORD_COUNT = RECORD_COUNT + 1
                BUFFER(RECORD_COUNT) = STRING
              ELSE
                DO I =  1, NREC
                  RECORD_COUNT = RECORD_COUNT + 1
                  BUFFER(RECORD_COUNT) = RECORD(I)
                ENDDO
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDDO
C
C ****  Place the new parameter(s) at the bottom if requested
C
  800 IF ( BOTTOM ) THEN
        DO I = 1, NREC
          RECORD_COUNT = RECORD_COUNT + 1
          BUFFER(RECORD_COUNT) = RECORD(I)
          FOUND = .TRUE.
        ENDDO
      ENDIF
C
C ****  Error message
C
      
      IF ( .NOT. FOUND ) THEN
        ERROR_MESSAGE = 'Element '//ARRAY_ELEMENT(1:LNAM)//
     &    ' NOT found !'//RECORD(1)(1:RLEN)//' NOT ADDED'
        CALL ERRMSG('PARAM NOT ADDED','EZ_ADD_PARAM',
     &    ERROR_MESSAGE(1:TRULEN(ERROR_MESSAGE)),'W')
        GOTO 999
      ENDIF
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
