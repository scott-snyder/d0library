      SUBROUTINE EZ_MODIFY_ELEMENT(ARRAY_NAME,ARRAY_ELEMENT,OCURRENCE,
     &  RECORD,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Modifies a record defined by the triplet
C-   (param-name,vlue,remark from an array following the structure:
C-      'Param-name'    value   'Remark'
C-              :       :       :
C-
C-   Inputs  : ARRAY_NAME    [C*]: Nane of the array
C-             ARRAY_ELEMENT [C*]: Name of the element to be modify
C-             OCURRENCE     [I ]: The nth occurrence of the element
C-                                    specified in ARRAY_ELEMENT
C-             RECORD        [C*]: Array of record(s) to be modify
C-
C-   Outputs : None
C-
C-   Created  31-MAY-1991   Lupe Howell
C-   Updated  19-JUN-1991   Lupe Howell  Tidy up
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) ARRAY_NAME
      CHARACTER*(*) ARRAY_ELEMENT
      CHARACTER*(*) RECORD
      INTEGER OCURRENCE,IER
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:SRCP.DEF'
      INCLUDE 'D0$INC:LKSRCP.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$OFFLINE_UTIL$GENERAL:VALUE.DEF'
C----------------------------------------------------------------------
      INTEGER MAXREC
      PARAMETER( MAXREC = 500 )
C
      INTEGER COUNT,ID,LPAR,LREM
      INTEGER TOTAL,IPOINT,VALPOINT,RECORD_COUNT
      INTEGER EZZSHFT,EZZAND,IPVAL,IPTV,IPTT,IPTO,IPTI
      INTEGER LRCP,LARR,LNAM,JVAL,LVAL,ITYP,I,J,II,JJ
C
      CHARACTER*32 NAME,RCPFILE
      CHARACTER*32 PARNAME
      CHARACTER*132 AVAL,REM,BUFFER(MAXREC),STRING
C
      LOGICAL ACTIVE
C
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
        IF ( IPOINT .GT. TOTAL ) GOTO 800
C
C ****  Get element of array (param, value, remark) and update pointer
C
        CALL EZ_GET_NEXT_ELEMENT(IPOINT,IC(IPTV),IC(IPTT),
     &    PARNAME,JVAL,AVAL,LVAL,ITYP,REM,VALPOINT)
        RECORD_COUNT = RECORD_COUNT + 1
C
C ****  Convert to string
C
        CALL SWORDS(PARNAME,II,JJ,LPAR)
        CALL SWORDS(REM,II,JJ,LREM)
        IF ( LREM .EQ. 0 ) LREM = 1
        CALL EZ_CVT_ELEMENT
     &    (PARNAME,LPAR,JVAL,AVAL,LVAL,REM,LREM,ITYP,
     &    BUFFER(RECORD_COUNT))
C
C ****  Comparing current parameter name with parameter name requested
C ****  If the counter equals to the number of ocurrence requested
C ****  replace the current recor with the new record
C
        IF ( ARRAY_ELEMENT(1:LNAM) .EQ. PARNAME(1:LNAM) ) THEN
          COUNT = COUNT + 1
          IF( COUNT .EQ. OCURRENCE ) THEN
              BUFFER(RECORD_COUNT) = RECORD
              IER = 0
          ENDIF
        ENDIF
      ENDDO
C
C ****  Setting up the buffer with '\END' keyword
C
      RECORD_COUNT = RECORD_COUNT + 1
      BUFFER(RECORD_COUNT) = '\END'
C
C ****  Recreate the array
C
  800 CALL EZ_MODIFY_ARRAY(ARRAY_NAME,BUFFER,RECORD_COUNT,IER)

  999 RETURN
      END
