      SUBROUTINE EZ_REMOVE_ELEMENT(ARRAY_NAME,ARRAY_ELEMENT,OCURRENCE,
     &   NREC,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Remove NREC records defined by the triplet
C-   (param-name,value,remark) from an array of the following
C-   structure:
C-
C-   \ARRAY array-name
C-      'Param-name'    value   'Remark'
C-              :       :       :
C-   \END
C-
C-   Inputs  : ARRAY_NAME    [C*] - Name of the array
C-             ARRAY_ELEMENT [C*] - Name of the element to remove
C-             OCURRENCE     [I ] - The nth occurrence of the element
C-                                  specified in ARRAY_ELEMENT
C-             NREC          [I ] - Number of records to be remove
C-
C-   Outputs : IER           [I ] - 0 - OK
C-
C-   Created 30-MAY-1991   Lupe Howell, Harrison B. Prosper
C-   Updated   5-SEP-1991   Lupe Howell  The remark size(1) of empty rem 
C-                         fixed to avoid errors
C-   Updated  30-OCT-1991   Lupe Howell  Tidy up skipping elements 
C-   Updated  11-DEC-1991   Lupe Howell  Make MAXREC larger 
C-   Updated  16-DEC-1991   Harrison B. Prosper  
C-      Use symbolic character length constants 
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
      PARAMETER( MAXREC = 900 )
C
      INTEGER COUNT,ID,II,JJ,LPAR,LREM
      INTEGER TOTAL,IPOINT,VALPOINT,RECORD_COUNT
      INTEGER LEN,EZZSHFT,EZZAND,IPVAL,IPTV,IPTT,IPTO,IPTI
      INTEGER LRCP,LARR,LNAM,JVAL,LVAL,ITYP,I,J,N
C
      CHARACTER*(NUMCHR) NAME,RCPFILE
      CHARACTER*(NUMCHR) PARNAME
      CHARACTER*(CHRCRD) AVAL,REM,BUFFER(MAXREC),STRING
C
      LOGICAL ACTIVE,GET,CHARS
C----------------------------------------------------------------------
      LARR = LEN(ARRAY_NAME)
      CALL SWORDS(ARRAY_ELEMENT,I,J,LNAM)
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
      DO WHILE ( ACTIVE )
C
C ****  Exit if we've gone beyond the given array
C
        IF ( IPOINT .GT. TOTAL ) GOTO 888 
C
C ****  Get element of array (param, value, remark) and update pointer
C
        CALL EZ_GET_NEXT_ELEMENT(IPOINT,IC(IPTV),IC(IPTT),
     &    PARNAME,JVAL,AVAL,LVAL,ITYP,REM,VALPOINT)
        RECORD_COUNT = RECORD_COUNT + 1
C
C ****  Convert to string and build buffer
C
        CALL SWORDS(PARNAME,II,JJ,LPAR)
        CALL SWORDS(REM,II,JJ,LREM)
        IF ( LREM .EQ. 0 ) LREM = 1
        CALL EZ_CVT_ELEMENT
     &    (PARNAME,LPAR,JVAL,AVAL,LVAL,REM,LREM,ITYP,
     &    BUFFER(RECORD_COUNT))
C
C ****  Comparing current parameter name with parameter name requested
C ****  If the counter equals to the number of ocurrence requested skip
C ****  NREC records
C
        IF ( ARRAY_ELEMENT(1:LNAM) .EQ. PARNAME(1:LNAM) ) THEN
          COUNT = COUNT + 1
          IF( COUNT .EQ. OCURRENCE ) THEN
            STRING = BUFFER(RECORD_COUNT)
            RECORD_COUNT = RECORD_COUNT - 1
C
C ****  Skip NREC records to remove them from the buffer
C
            I = 0
            DO WHILE( (I .LT. (NREC-1)) .AND. (IPOINT .LT. TOTAL ) )
              I = I + 1
              CALL EZ_GET_NEXT_ELEMENT(IPOINT,IC(IPTV),IC(IPTT),
     &          PARNAME,JVAL,AVAL,LVAL,ITYP,REM,VALPOINT)
            ENDDO
          ENDIF
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
