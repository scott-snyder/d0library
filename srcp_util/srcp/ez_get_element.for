      SUBROUTINE EZ_GET_ELEMENT(ARRAY_NAME,ARRAY_ELEMENT,IDX,OCURRENCE,
     &            VAL,VALTYPE,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get the value of an element,
C-   defined by the triplet
C-   (param-name,value,remark) from an array of the following
C-   structure:
C-
C-   \ARRAY array-name
C-      'Param-name'    value   'Remark'
C-              :       :       :
C-   \END
C-
C-   The search for the requested element will begin at the IDX word
C-   of the array ARRAY_NAME and will return the value of the NTH OCCURRENCE
C-   of that element.
C-
C-   Inputs  : ARRAY_NAME    [C*] - Name of the array
C-             ARRAY_ELEMENT [C*] - Name of the element in the array to find
C-             IDX           [I ] - Index in the array where to start the
C-                                  search
C-             OCURRENCE     [I ] - The number of nth occurrence of the element
C-                                  that will be return
C-
C-   Outputs : VAL           [I(*)] - Value of the element requested
C-             VALTYPE       [I ] - Type of the value
C-             IER           [I ] - If it does not equal to 0 the element was
C-                                  not found
C-
C-   Created   8-MAY-1990   Lupe Howell
C-   Updated  17-OCT-1990   Lupe Howell  EZ_SET_ELEMENT entry created
C-   Updated  23-JAN-1991   Lupe Howell  Check with old array name remove
C-              because array names can be the same in  different RCP files.
C-   Updated   9-APR-1991   Lupe Howell, Harrison B. Prosper The bank
C-            manipulation is done using poiters not local arrays.
C-   Updated  29-MAY-1991   Lupe Howell  GET/SET character parameters with size
C-                          greather than four
C-   Updated  21-JUN-1991   Harrison B. Prosper  
C-      Add entry point to return remark 
C-   Updated   3-DEC-1991   Lupe Howell  Add entry point to return the
C-      character value
C-   Updated  16-DEC-1991   Harrison B. Prosper  
C-      Use symbolic character lengths (NUMCHR and CHRCRD) 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) ARRAY_NAME
      CHARACTER*(*) ARRAY_ELEMENT
      INTEGER VALTYPE,IDX, OCURRENCE, VAL(*), IER
      CHARACTER*(*) REMARK
      CHARACTER*(*)CVALUE
C----------------------------------------------------------------------
      INCLUDE 'D0$PARAMS:SRCP.DEF'
      INCLUDE 'D0$INC:LKSRCP.INC'
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$OFFLINE_UTIL$GENERAL:VALUE.DEF'
C----------------------------------------------------------------------
      INTEGER COUNT,ID
      INTEGER TOTAL,IPOINT,VALPOINT
      INTEGER LEN,EZZSHFT,EZZAND,IPVAL,IPTV,IPTT,IPTO,IPTI
      INTEGER LRCP,LARR,LNAM,JVAL,LVAL,ITYP,I,J,CLEN,LENGTH,N
C 
      CHARACTER*(NUMCHR) NAME,RCPFILE
      CHARACTER*(NUMCHR) PARNAME
      CHARACTER*(CHRCRD) AVAL,REM
C
      LOGICAL ACTIVE,GET,CHARS
C----------------------------------------------------------------------
      SAVE REM,AVAL
C----------------------------------------------------------------------
      GET = .TRUE.
      GOTO 20
C
      ENTRY EZ_SET_ELEMENT(ARRAY_NAME,ARRAY_ELEMENT,IDX,OCURRENCE,
     &      VAL,IER)
      GET = .FALSE.
C
   20 CONTINUE
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
      IPOINT = IDX
      COUNT  = 0
      IER    = 0
C
C ****  Searching for the Nth element requested
C
      IER = -1                          ! Assume it does not finds it
      ACTIVE = .TRUE.
      DO WHILE ( ACTIVE )
C
C ****  Exit if we've gone beyond the given array
C
        IF ( IPOINT .GT. TOTAL ) GOTO 999
C
C ****  Get element of array (param, value, remark) and update pointer
C
        CALL EZ_GET_NEXT_ELEMENT(IPOINT,IC(IPTV),IC(IPTT),
     &    PARNAME,JVAL,AVAL,LVAL,ITYP,REM,VALPOINT)
C
C ****  Comparing current parameter name with parameter name requested
C ****  If the counter equals to the number of ocurrence requested get/set
C ****  the parameter
C
        IF ( ARRAY_ELEMENT(1:LNAM) .EQ. PARNAME(1:LNAM) ) THEN
          COUNT = COUNT + 1
          IF( COUNT .EQ. OCURRENCE ) THEN
            IF ( GET ) THEN
C
C ****  If getting a character parameter convert the characters into 
C ****  hollorith number and return it in VAL
C
              IF ( ITYP .GE. VTCHAR ) THEN
                CALL DCTOH(LVAL,AVAL,VAL)
              ELSE
C
C ****  If getting a non character parameter return the value found
C
                VAL(1) = JVAL
              ENDIF
              VALTYPE = ITYP
              IER = 0
C
C ****  If setting a character parameter set the new value taking in 
C ****  account the size of the character 
C
            ELSEIF ( ITYP .GE. VTCHAR ) THEN
              LENGTH = ITYP - 10
              N = ( LENGTH+3)/4
              CALL EZSET1(ID,VALPOINT,VALPOINT+N-1,1,VAL,IER)
            ELSE
C
C ****  If setting a non character parameter set the new value
C
              CALL EZSET1(ID,VALPOINT,VALPOINT,1,VAL,IER)
            ENDIF
C
C ****  Exit if counter equals the number of ocurrence requested
C
            ACTIVE = .FALSE.
          ENDIF
        ENDIF
      ENDDO
      RETURN
C
      ENTRY EZ_GET_ELEMENT_REMARK(REMARK)
      REMARK = REM
      RETURN
C
      ENTRY EZ_GET_ELEMENT_CHARACTER(CVALUE)
      CVALUE = AVAL(1:LVAL) 
  999 RETURN
      END
